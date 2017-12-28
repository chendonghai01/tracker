#include "config.h"

#include <string.h>

#include "tracker-string-builder.h"

typedef struct _TrackerStringChunk TrackerStringChunk;
typedef struct _TrackerStringElement TrackerStringElement;

struct _TrackerStringChunk
{
	gchar *string;
	gsize allocated_size;
	gsize len;
};

enum {
	ELEM_TYPE_STRING,
	ELEM_TYPE_BUILDER
};

struct _TrackerStringElement
{
	guint type;
	union {
		TrackerStringChunk *chunk;
		TrackerStringBuilder *builder;
	} data;
};

struct _TrackerStringBuilder
{
	GObject parent_instance;
	GArray *elems;
};

struct _TrackerStringBuilderClass
{
	GObjectClass parent_class;
};

G_DEFINE_TYPE (TrackerStringBuilder, tracker_string_builder, G_TYPE_OBJECT)

static void
tracker_string_builder_finalize (GObject *object)
{
	TrackerStringBuilder *builder = TRACKER_STRING_BUILDER (object);

	g_array_free (builder->elems, TRUE);

	G_OBJECT_CLASS (tracker_string_builder_parent_class)->finalize (object);
}

static void
tracker_string_builder_class_init (TrackerStringBuilderClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = tracker_string_builder_finalize;
}

static void
free_string_chunk (TrackerStringChunk *chunk)
{
	g_free (chunk->string);
	g_free (chunk);
}

static void
free_string_element (gpointer data)
{
	TrackerStringElement *elem = data;

	if (elem->type == ELEM_TYPE_STRING)
		free_string_chunk (elem->data.chunk);
	else if (elem->type == ELEM_TYPE_BUILDER)
		g_object_unref (elem->data.builder);
}

static void
tracker_string_builder_init (TrackerStringBuilder *builder)
{
	builder->elems = g_array_new (FALSE, TRUE, sizeof (TrackerStringElement));
	g_array_set_clear_func (builder->elems, free_string_element);
}

TrackerStringBuilder *
tracker_string_builder_new (void)
{
	return g_object_new (TRACKER_TYPE_STRING_BUILDER, NULL);
}

TrackerStringBuilder *
tracker_string_builder_append_placeholder (TrackerStringBuilder *builder)
{
	TrackerStringBuilder *child;
	TrackerStringElement elem;

	child = tracker_string_builder_new ();

	elem.type = ELEM_TYPE_BUILDER;
	elem.data.builder = child;
	g_array_append_val (builder->elems, elem);

	return g_object_ref (child);
}

TrackerStringBuilder *
tracker_string_builder_prepend_placeholder (TrackerStringBuilder *builder)
{
	TrackerStringBuilder *child;
	TrackerStringElement elem;

	child = tracker_string_builder_new ();

	elem.type = ELEM_TYPE_BUILDER;
	elem.data.builder = child;
	g_array_prepend_val (builder->elems, elem);

	return g_object_ref (child);
}

static TrackerStringChunk *
ensure_last_chunk (TrackerStringBuilder *builder)
{
	TrackerStringElement elem;
	TrackerStringChunk *chunk;

	if (builder->elems->len > 0) {
		TrackerStringElement *last;

		last = &g_array_index (builder->elems, TrackerStringElement,
		                       builder->elems->len - 1);
		if (last->type == ELEM_TYPE_STRING)
			return last->data.chunk;
	}

	chunk = g_new0 (TrackerStringChunk, 1);

	elem.type = ELEM_TYPE_STRING;
	elem.data.chunk = chunk;
	g_array_append_val (builder->elems, elem);

	return chunk;
}

static TrackerStringChunk *
ensure_first_chunk (TrackerStringBuilder *builder)
{
	TrackerStringElement elem;
	TrackerStringChunk *chunk;

	/* Always create a new element instead of trying to prepend on
	 * the first string chunk. Between memory relocations and memory
	 * fragmentation, we choose the latter. This object is short lived
	 * anyway.
	 */
	chunk = g_new0 (TrackerStringChunk, 1);

	elem.type = ELEM_TYPE_STRING;
	elem.data.chunk = chunk;
	g_array_prepend_val (builder->elems, elem);

	return chunk;
}

static inline gsize
fitting_power_of_two (gsize string_len)
{
	gsize s = 1;

	while (s < string_len)
		s <<= 1;

	return s;
}

static void
string_chunk_append (TrackerStringChunk *chunk,
                     const gchar        *str,
                     gssize              len)
{
	if (len < 0)
		len = strlen (str);

	if (chunk->len + len > chunk->allocated_size) {
		/* Expand size */
		gssize new_size = fitting_power_of_two (chunk->len + len);

		g_assert (new_size > chunk->allocated_size);
		chunk->string = g_realloc (chunk->string, new_size);
		chunk->allocated_size = new_size;
	}

	/* String (now) fits in allocated size */
	strncpy (&chunk->string[chunk->len], str, len);
}

void
tracker_string_builder_append (TrackerStringBuilder *builder,
                               const gchar          *string,
                               gssize                len)
{
	TrackerStringChunk *chunk;

	chunk = ensure_last_chunk (builder);
	string_chunk_append (chunk, string, len);
}

void
tracker_string_builder_prepend (TrackerStringBuilder *builder,
                                const gchar          *string,
                                gssize                len)
{
	TrackerStringChunk *chunk;

	chunk = ensure_first_chunk (builder);
	string_chunk_append (chunk, string, len);
}

void
tracker_string_builder_append_printf (TrackerStringBuilder *builder,
                                      const gchar          *format,
                                      ...)
{
	TrackerStringChunk *chunk;
	va_list varargs;
	gchar *str;

	va_start (varargs, format);
	str = g_strdup_vprintf (format, varargs);
	va_end (varargs);

	chunk = ensure_last_chunk (builder);
	string_chunk_append (chunk, str, -1);
	g_free (str);
}

void
tracker_string_builder_prepend_printf (TrackerStringBuilder *builder,
                                       const gchar          *format,
                                       ...)
{
	TrackerStringChunk *chunk;
	va_list varargs;
	gchar *str;

	va_start (varargs, format);
	str = g_strdup_vprintf (format, varargs);
	va_end (varargs);

	chunk = ensure_first_chunk (builder);
	string_chunk_append (chunk, str, -1);
	g_free (str);
}

static void
tracker_string_builder_to_gstring (TrackerStringBuilder *builder,
                                   GString              *str)
{
	guint i;

	for (i = 0; i < builder->elems->len; i++) {
		TrackerStringElement *elem;

		elem = &g_array_index (builder->elems, TrackerStringElement, i);

		if (elem->type == ELEM_TYPE_STRING) {
			g_string_append_len (str,
			                     elem->data.chunk->string,
			                     elem->data.chunk->len);
		} else if (elem->type == ELEM_TYPE_BUILDER) {
			tracker_string_builder_to_gstring (elem->data.builder,
			                                   str);
		}
	}
}

gchar *
tracker_string_builder_to_string (TrackerStringBuilder *builder)
{
	GString *str = g_string_new (NULL);

	tracker_string_builder_to_gstring (builder, str);

	return g_string_free (str, FALSE);
}
