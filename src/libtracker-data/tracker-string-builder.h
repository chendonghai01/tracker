#ifndef __TRACKER_STRING_BUILDER_H__
#define __TRACKER_STRING_BUILDER_H__

#include <glib-object.h>

#define TRACKER_TYPE_STRING_BUILDER (tracker_string_builder_get_type ())

G_DECLARE_FINAL_TYPE (TrackerStringBuilder, tracker_string_builder,
                      TRACKER, STRING_BUILDER, GObject)

TrackerStringBuilder * tracker_string_builder_new (void);
TrackerStringBuilder * tracker_string_builder_append_placeholder  (TrackerStringBuilder *builder);
TrackerStringBuilder * tracker_string_builder_prepend_placeholder (TrackerStringBuilder *builder);

void tracker_string_builder_append  (TrackerStringBuilder *builder,
                                     const gchar          *string,
                                     gssize                len);
void tracker_string_builder_prepend (TrackerStringBuilder *builder,
                                     const gchar          *string,
                                     gssize                len);
void tracker_string_builder_append_printf  (TrackerStringBuilder *builder,
                                            const gchar          *format,
                                            ...);
void tracker_string_builder_prepend_printf (TrackerStringBuilder *builder,
                                            const gchar          *format,
                                            ...);

gchar * tracker_string_builder_to_string (TrackerStringBuilder *builder);

#endif /* __TRACKER_STRING_BUILDER_H__ */
