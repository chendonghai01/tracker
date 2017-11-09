#ifndef __TRACKER_SPARQL_PARSER_H__
#define __TRACKER_SPARQL_PARSER_H__

#include <glib.h>

typedef struct _TrackerParserNode TrackerParserNode;
typedef struct _TrackerGrammarRule TrackerGrammarRule;

GNode * tracker_sparql_parse_query  (const gchar  *query,
                                     gssize        len,
                                     gsize        *len_out,
                                     GError      **error);
GNode * tracker_sparql_parse_update (const gchar  *query,
                                     gssize        len,
                                     gsize        *len_out,
                                     GError      **error);

void tracker_sparql_parser_tree_free (GNode *node);
GNode * tracker_sparql_parser_tree_find_first (GNode *node);
GNode * tracker_sparql_parser_tree_find_next  (GNode *node);

const TrackerGrammarRule * tracker_parser_node_get_rule (TrackerParserNode *node);

gboolean tracker_parser_node_get_extents (TrackerParserNode *node,
                                          gssize            *start,
                                          gssize            *end);

#endif /* __TRACKER_SPARQL_PARSER_H__ */
