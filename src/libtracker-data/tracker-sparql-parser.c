#include "config.h"

#include "tracker-sparql-query.h"
#include "tracker-sparql-parser.h"
#include "tracker-sparql-grammar.h"

#include "string.h"

typedef struct _TrackerParserNode TrackerParserNode;
typedef struct _TrackerParserState TrackerParserState;
typedef struct _TrackerGrammarParser TrackerGrammarParser;

struct _TrackerParserNode {
	const TrackerGrammarRule *rule;
	gssize start;
	gssize end;
	guint n_children;
	gint cur_child;
};

struct _TrackerParserState {
	GNode *expression_tree;
	GNode *current_rule_node;
	gssize current;
	GError *error;
};

struct _TrackerGrammarParser {
	const gchar *query;
	gsize query_len;
};

static void tracker_grammar_rule_print_helper (GString                  *str,
					       const TrackerGrammarRule *rule,
					       gint                      depth);

static void
tracker_grammar_rule_print_children (GString                  *str,
				     const TrackerGrammarRule *rules,
				     const gchar              *start,
				     const gchar              *sep,
				     const gchar              *end,
				     gint                      depth)
{
	gint i;

	g_string_append (str, start);

	for (i = 0; rules[i].type != RULE_TYPE_NIL; i++) {
		if (i != 0)
			g_string_append (str, sep);
		tracker_grammar_rule_print_helper (str, &rules[i], depth);
	}

	g_string_append (str, end);
}

static void
tracker_grammar_rule_print_helper (GString                  *str,
				   const TrackerGrammarRule *rule,
				   gint                      depth)
{
	if (depth == 0) {
		g_string_append (str, "…");
		return;
	}

	depth--;

	switch (rule->type) {
	case RULE_TYPE_LITERAL:
		g_string_append_printf (str, "'%s'", rule->string);
		break;
	case RULE_TYPE_RULE:
	case RULE_TYPE_TERMINAL:
		g_string_append_printf (str, "%s", rule->string);
		break;
	case RULE_TYPE_SEQUENCE:
		tracker_grammar_rule_print_children (str, rule->data.children,
						     "(", " ", ")", depth);
		break;
	case RULE_TYPE_OR:
		tracker_grammar_rule_print_children (str, rule->data.children,
						     "(", " | ", ")", depth);
		break;
	case RULE_TYPE_GTE0:
		tracker_grammar_rule_print_children (str, rule->data.children,
						     "(", " ", ")+", depth);
		break;
	case RULE_TYPE_GT0:
		tracker_grammar_rule_print_children (str, rule->data.children,
						     "(", " ", ")*", depth);
		break;
	case RULE_TYPE_OPTIONAL:
		tracker_grammar_rule_print_children (str, rule->data.children,
						     "(", " ", ")?", depth);
		break;
	}
}

static gchar *
tracker_grammar_rule_print (const TrackerGrammarRule *rule)
{
	GString *str;

	str = g_string_new (NULL);
	tracker_grammar_rule_print_helper (str, rule, 3);
	return g_string_free (str, FALSE);
}

static guint
count_child_rules (const TrackerGrammarRule *rule)
{
	const TrackerGrammarRule *children;
	guint i = 0;

	children = tracker_grammar_rule_get_children (rule);

	if (!children)
		return 0;

	while (children[i].type != RULE_TYPE_NIL)
		i++;

	return i;
}

static TrackerParserNode *
tracker_parser_node_new (const TrackerGrammarRule *rule,
                         const TrackerParserState *state)
{
	TrackerParserNode *node;

	node = g_new0 (TrackerParserNode, 1);
	node->rule = rule;
	node->start = node->end = state->current;

	switch (rule->type) {
	case RULE_TYPE_RULE:
	case RULE_TYPE_SEQUENCE:
	case RULE_TYPE_GT0:
	case RULE_TYPE_GTE0:
	case RULE_TYPE_OPTIONAL:
	case RULE_TYPE_OR:
		node->n_children = count_child_rules (rule);
		node->cur_child = -1;
		break;
	default:
		break;
	}

	return node;
}

static void
tracker_parser_node_free (TrackerParserNode *node)
{
	g_free (node);
}

static void
tracker_parser_node_set_end (TrackerParserNode    *node,
                             TrackerGrammarParser *parser,
                             gssize                pos)
{
	while (pos > node->start) {
		if (parser->query[pos - 1] != ' ' &&
		    parser->query[pos - 1] != '\n' &&
		    parser->query[pos - 1] != '\t')
			break;
		pos--;
	}

	node->end = pos;
}

TrackerGrammarParser *
tracker_grammar_parser_new (const gchar *query,
                            gsize        len)
{
	TrackerGrammarParser *parser;

	parser = g_new0 (TrackerGrammarParser, 1);
	parser->query = query;
	parser->query_len = len;

	return parser;
}

static void
tracker_grammar_parser_free (TrackerGrammarParser *parser)
{
	g_free (parser);
}

static void
tracker_parser_state_take_error (TrackerParserState *state,
				 GError             *error)
{
	if (state->error)
		g_error_free (state->error);
	state->error = error;
}

static void
tracker_parser_state_forward (TrackerParserState   *state,
			      TrackerGrammarParser *parser,
			      gssize                len)
{
	g_assert (len >= 0 && state->current + len <= parser->query_len);
	state->current += len;
}

static void
tracker_parser_state_rewind (TrackerParserState *state,
			     gssize              pos)
{
	g_assert (pos >= 0 && pos <= state->current);
	state->current = pos;
}

static void
tracker_parser_state_skip_whitespace (TrackerParserState   *state,
				      TrackerGrammarParser *parser)
{
	while (state->current < parser->query_len) {
		if (parser->query[state->current] != ' ' &&
		    parser->query[state->current] != '\n' &&
		    parser->query[state->current] != '\t')
			break;

		tracker_parser_state_forward (state, parser, 1);
	}
}

static gboolean
tracker_grammar_parser_apply_rule_nested (TrackerGrammarParser     *parser,
                                          TrackerParserState       *state,
                                          const TrackerGrammarRule *rule)
{
	const TrackerGrammarRule *children;
	GNode *child;

	children = tracker_grammar_rule_get_children (rule);

	/* Initialize a new expression tree node */
	child = g_node_new (tracker_parser_node_new (&children[0], state));
	g_node_append (state->current_rule_node, child);

	return TRUE;
}

static gboolean
tracker_grammar_parser_apply_rule_literal (TrackerGrammarParser     *parser,
                                           TrackerParserState       *state,
                                           const TrackerGrammarRule *rule)
{
	gsize len = strlen (rule->string);

	if (state->current + len > parser->query_len ||
	    g_ascii_strncasecmp (rule->string, &parser->query[state->current], len) != 0 ||
	    (g_ascii_isalnum (rule->string[0]) &&
	     g_ascii_isalnum (parser->query[state->current + len]))) {
		tracker_parser_state_take_error (state,
						 g_error_new (TRACKER_SPARQL_ERROR,
							      TRACKER_SPARQL_ERROR_PARSE,
							      "Expected literal '%s'", rule->string));
		return FALSE;
	}

	tracker_parser_state_forward (state, parser, len);
	return TRUE;
}

static gboolean
tracker_grammar_parser_apply_rule_terminal (TrackerGrammarParser     *parser,
                                            TrackerParserState       *state,
                                            const TrackerGrammarRule *rule)
{
	TrackerTerminalFunc func;
	const gchar *str, *end;

	str = &parser->query[state->current];

	if (state->current == parser->query_len || str[0] == '\0') {
		tracker_parser_state_take_error (state,
						 g_error_new (TRACKER_SPARQL_ERROR,
							      TRACKER_SPARQL_ERROR_PARSE,
							      "Incomplete input"));
		return FALSE;
	}

	func = tracker_grammar_rule_get_terminal_func (rule);

	if (!func (str, &parser->query[parser->query_len], &end)) {
		tracker_parser_state_take_error (state,
						 g_error_new (TRACKER_SPARQL_ERROR,
							      TRACKER_SPARQL_ERROR_PARSE,
							      "Expected terminal '%s'", rule->string));
		return FALSE;
	}

	tracker_parser_state_forward (state, parser, end - str);
	return TRUE;
}

static gboolean
tracker_grammar_parser_apply_rule (TrackerGrammarParser     *parser,
                                   TrackerParserState       *state,
                                   const TrackerGrammarRule *rule)
{
	tracker_parser_state_skip_whitespace (state, parser);

	switch (rule->type) {
	case RULE_TYPE_RULE:
	case RULE_TYPE_SEQUENCE:
	case RULE_TYPE_GT0:
	case RULE_TYPE_GTE0:
	case RULE_TYPE_OPTIONAL:
	case RULE_TYPE_OR:
		return tracker_grammar_parser_apply_rule_nested (parser,
								 state, rule);
	case RULE_TYPE_LITERAL:
		return tracker_grammar_parser_apply_rule_literal (parser,
								  state, rule);
	case RULE_TYPE_TERMINAL:
		return tracker_grammar_parser_apply_rule_terminal (parser,
								   state, rule);
	case RULE_TYPE_NIL:
		g_assert_not_reached ();
		return FALSE;
	}
}

static void
tracker_parser_node_next_child (TrackerParserNode *node,
				gboolean           success)
{
	if (success) {
		const TrackerGrammarRule *rule = node->rule;

		if (rule->type == RULE_TYPE_OR) {
			/* Successful OR rules are satisfied already */
			node->cur_child = node->n_children;
			return;
		} else if (rule->type == RULE_TYPE_GT0 ||
			   rule->type == RULE_TYPE_GTE0) {
			/* Successful + and * rules are evaluated again */
			return;
		}
	}

	node->cur_child++;
}

static GNode *
create_child_node (GNode                *parent,
                   TrackerParserState   *state,
                   TrackerGrammarParser *parser)
{
	const TrackerGrammarRule *children;
	TrackerParserNode *data;
	GNode *child;

	data = parent->data;

	if (data->cur_child >= data->n_children)
		return NULL;

	tracker_parser_state_skip_whitespace (state, parser);

	children = tracker_grammar_rule_get_children (data->rule);
	child = g_node_new (tracker_parser_node_new (&children[data->cur_child], state));
	g_node_append (parent, child);

	return child;
}

static gboolean
tracker_parser_state_iterate (TrackerParserState   *state,
                              TrackerGrammarParser *parser)
{
	TrackerParserNode *data;
	GNode *node, *parent;

	node = state->current_rule_node;
	data = node->data;
	tracker_parser_node_set_end (data, parser, state->current);

	/* Iterate into first child if we didn't recurse into this node yet */
	if (data->n_children > 0 && data->cur_child < 0) {
		state->current_rule_node = g_node_first_child (state->current_rule_node);
		data->cur_child = 0;
		return TRUE;
	}

	/* Find the first parent that has a next child to handle */
	while (node) {
		GNode *next = NULL;

		parent = node->parent;

		if (parent) {
			TrackerParserNode *parent_data = parent->data;

			parent_data->end = data->end;
			tracker_parser_node_next_child (parent_data, TRUE);
			next = create_child_node (parent, state, parser);
		}

		if (next) {
			state->current_rule_node = next;
			return TRUE;
		}

		node = parent;
	}

	state->current_rule_node = NULL;
	return FALSE;
}

static void
tracker_parser_state_set_error_for_rule (TrackerParserState       *state,
					 const TrackerGrammarRule *rule)
{
	gchar *expected;

	expected = tracker_grammar_rule_print (rule);
	tracker_parser_state_take_error (state,
					 g_error_new (TRACKER_SPARQL_ERROR,
						      TRACKER_SPARQL_ERROR_PARSE,
						      "Expected %s", expected));
	g_free (expected);
}

static gboolean
tracker_parser_state_rollback (TrackerParserState   *state,
                               TrackerGrammarParser *parser)
{
	GNode *node, *parent, *next, *discard = NULL;
	gboolean retval = FALSE;

	node = state->current_rule_node;

	while (node && node->parent && !retval) {
		TrackerParserNode *parent_data, *data;

		parent = node->parent;
		parent_data = parent->data;
		data = node->data;

		/* Reset state to retry again the failed portions */
		tracker_parser_state_rewind (state, data->start);

		switch (parent_data->rule->type) {
		case RULE_TYPE_OR:
			/* Iterate to next value, discard previous one. If no
			 * next value is found, raise an error.
			 */
			tracker_parser_node_next_child (parent_data, FALSE);
			next = create_child_node (parent, state, parser);

			if (next) {
				state->current_rule_node = next;
				discard = node;
				retval = TRUE;
			} else {
				tracker_parser_state_set_error_for_rule (state, parent_data->rule);
			}
			break;
		case RULE_TYPE_GT0:
			/* If we errored out the first time we
			 * parse ()+, raise an error.
			 */
			if (!node->prev ||
			    ((TrackerParserNode *) node->data)->rule !=
			    ((TrackerParserNode *) node->prev->data)->rule) {
				tracker_parser_state_set_error_for_rule (state, parent_data->rule);
				break;
			}

			/* Fall through */
		case RULE_TYPE_GTE0:
		case RULE_TYPE_OPTIONAL:
			retval = TRUE;
			discard = node;

			while (parent) {
				tracker_parser_node_set_end (parent->data, parser, state->current);

				if (parent->parent) {
					tracker_parser_node_next_child (parent->parent->data, TRUE);
					state->current_rule_node = create_child_node (parent->parent, state, parser);
				}

				if (state->current_rule_node)
					break;

				parent = parent->parent;
			}

			break;
		default:
			discard = node;
			break;
		}

		node = node->parent;
	}

	if (discard)
		tracker_sparql_parser_tree_free (discard);

	return retval;
}

static gboolean
tracker_grammar_parser_read (TrackerGrammarParser *parser,
                             TrackerParserState   *state)
{
	while (state->current_rule_node) {
		TrackerParserNode *node;

		node = state->current_rule_node->data;

		if (tracker_grammar_parser_apply_rule (parser, state,
		                                       node->rule)) {
			if (!tracker_parser_state_iterate (state, parser))
				break;
		} else {
			if (!tracker_parser_state_rollback (state, parser))
				break;

			/* We rolled back successfully, keep going. */
			tracker_parser_state_take_error (state, NULL);
		}
	}

	return state->error == NULL;
}

GNode *
tracker_grammar_parser_apply (TrackerGrammarParser      *parser,
                              const TrackerGrammarRule  *rule,
                              gsize                     *len_out,
                              GError                   **error)
{
	TrackerParserState state = { 0 };

	state.expression_tree = g_node_new (tracker_parser_node_new (rule, &state));
	state.current_rule_node = state.expression_tree;

	if (!tracker_grammar_parser_read (parser, &state)) {
		g_propagate_prefixed_error (error, state.error,
		                            "Parser error at byte %d:",
		                            state.current);
		return NULL;
	}

	if (len_out)
		*len_out = state.current;

	return state.expression_tree;
}

GNode *
tracker_sparql_parse_query (const gchar  *query,
                            gssize        len,
                            gsize        *len_out,
                            GError      **error)
{
	TrackerGrammarParser *parser;
	GNode *tree;

	g_return_val_if_fail (query != NULL, NULL);

	if (len < 0)
		len = strlen (query);

	parser = tracker_grammar_parser_new (query, len);
	tree = tracker_grammar_parser_apply (parser, NAMED_RULE (QueryUnit), len_out, error);
	tracker_grammar_parser_free (parser);

	return tree;
}

GNode *
tracker_sparql_parse_update (const gchar  *query,
                             gssize        len,
                             gsize        *len_out,
                             GError      **error)
{
	TrackerGrammarParser *parser;
	GNode *tree;

	g_return_val_if_fail (query != NULL, NULL);

	if (len < 0)
		len = strlen (query);

	parser = tracker_grammar_parser_new (query, len);
	tree = tracker_grammar_parser_apply (parser, NAMED_RULE (UpdateUnit), len_out, error);
	tracker_grammar_parser_free (parser);

	return tree;
}

const TrackerGrammarRule *
tracker_parser_node_get_rule (TrackerParserNode *node)
{
	return node->rule;
}

gboolean
tracker_parser_node_get_extents (TrackerParserNode *node,
				 gssize            *start,
				 gssize            *end)
{
	if (start)
		*start = node->start;
	if (end)
		*end = node->end;

	return node->end != node->start;
}

static gboolean
node_free (GNode    *node,
           gpointer  data)
{
	tracker_parser_node_free (node->data);
	return FALSE;
}

void
tracker_sparql_parser_tree_free (GNode *node)
{
	g_node_unlink (node);
	g_node_traverse (node,
			 G_PRE_ORDER,
			 G_TRAVERSE_ALL,
			 -1, node_free, NULL);
	g_node_destroy (node);
}

GNode *
tracker_sparql_parser_tree_find_first (GNode *node)
{
	GNode *first_leave = node;

	g_return_val_if_fail (node != NULL, NULL);

	while (first_leave->children)
		first_leave = first_leave->children;

	if (tracker_parser_node_get_extents (first_leave->data, NULL, NULL)) {
		return first_leave;
	} else {
		/* First leave is empty, find the first non-empty one */
		return tracker_sparql_parser_tree_find_next (first_leave);
	}
}

GNode *
tracker_sparql_parser_tree_find_next (GNode *node)
{
	g_return_val_if_fail (node != NULL, NULL);

	while (node) {
		if (node->next &&
		    tracker_parser_node_get_extents (node->next->data, NULL, NULL)) {
			return tracker_sparql_parser_tree_find_first (node->next);
		}

		node = node->next ? node->next : node->parent;
	}

	return NULL;
}
