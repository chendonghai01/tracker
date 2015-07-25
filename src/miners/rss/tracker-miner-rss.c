/*
 * Copyright (C) 2009/2010, Roberto Guido <madbob@users.barberaware.org>
 *                          Michele Tameni <michele@amdplanet.it>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA  02110-1301, USA.
 */

#include "config.h"

#include <stdio.h>

#include <libgrss.h>

#include <libtracker-common/tracker-dbus.h>
#include <libtracker-sparql/tracker-ontologies.h>
#include <libtracker-common/tracker-common.h>

#include <glib/gi18n.h>

#include "tracker-miner-rss.h"

#define TRACKER_MINER_RSS_GET_PRIVATE(obj) (G_TYPE_INSTANCE_GET_PRIVATE ((obj), TRACKER_TYPE_MINER_RSS, TrackerMinerRSSPrivate))

typedef struct _TrackerMinerRSSPrivate TrackerMinerRSSPrivate;

struct _TrackerMinerRSSPrivate {
	gboolean paused;
	gboolean stopped;
	gchar *last_status;

	GrssFeedsPool *pool;
	gint now_fetching;
	GDBusConnection *connection;
	guint graph_updated_id;

	GList *item_inserts;
	GHashTable *channel_updates;
	GHashTable *channels;

	gint rdf_type_id;
	gint mfo_feed_channel_id;
};

typedef struct {
	TrackerMinerRSS *miner;
	GrssFeedChannel *channel;
	gint timeout_id;
	GCancellable *cancellable;
} FeedChannelUpdateData;

typedef struct {
	TrackerMinerRSS *miner;
	GrssFeedItem *item;
	GCancellable *cancellable;
} FeedItemInsertData;

static void         graph_updated_cb                (GDBusConnection       *connection,
                                                     const gchar           *sender_name,
                                                     const gchar           *object_path,
                                                     const gchar           *interface_name,
                                                     const gchar           *signal_name,
                                                     GVariant              *parameters,
                                                     gpointer               user_data);
static void         miner_started                   (TrackerMiner          *miner);
static void         miner_stopped                   (TrackerMiner          *miner);
static void         miner_paused                    (TrackerMiner          *miner);
static void         miner_resumed                   (TrackerMiner          *miner);
static void         retrieve_and_schedule_feeds     (TrackerMinerRSS       *miner,
                                                     GArray                *channel_ids);
static gboolean     feed_channel_changed_timeout_cb (gpointer               user_data);
static void         feed_channel_update_data_free   (FeedChannelUpdateData *fcud);
static void         feed_item_insert_data_free      (FeedItemInsertData    *fiid);
static void         feed_fetching_cb                (GrssFeedsPool             *pool,
                                                     GrssFeedChannel           *feed,
                                                     gpointer               user_data);
static void         feed_ready_cb                   (GrssFeedsPool             *pool,
                                                     GrssFeedChannel           *feed,
                                                     GList                 *items,
                                                     gpointer               user_data);
static const gchar *get_message_url                 (GrssFeedItem              *item);

G_DEFINE_TYPE (TrackerMinerRSS, tracker_miner_rss, TRACKER_TYPE_MINER_ONLINE)

static void
parser_characters (void          *data,
                   const xmlChar *ch,
                   int            len)
{
	GString *string = data;
	const gchar *str, *end;

	str = ch;
	g_utf8_validate (str, len, &end);

	if (end > str) {
		g_string_append_len (string, str, end - str);
	}

	if (string->str[string->len - 1] != ' ')
		g_string_append_c (string, ' ');
}

static gchar *
parse_html_text (const gchar *html)
{
	GString *string;
	htmlDocPtr doc;
	xmlSAXHandler handler = {
		NULL, /* internalSubset */
		NULL, /* isStandalone */
		NULL, /* hasInternalSubset */
		NULL, /* hasExternalSubset */
		NULL, /* resolveEntity */
		NULL, /* getEntity */
		NULL, /* entityDecl */
		NULL, /* notationDecl */
		NULL, /* attributeDecl */
		NULL, /* elementDecl */
		NULL, /* unparsedEntityDecl */
		NULL, /* setDocumentLocator */
		NULL, /* startDocument */
		NULL, /* endDocument */
		NULL, /* startElement */
		NULL, /* endElement */
		NULL, /* reference */
		parser_characters, /* characters */
		NULL, /* ignorableWhitespace */
		NULL, /* processingInstruction */
		NULL, /* comment */
		NULL, /* xmlParserWarning */
		NULL, /* xmlParserError */
		NULL, /* xmlParserError */
		NULL, /* getParameterEntity */
		NULL, /* cdataBlock */
		NULL, /* externalSubset */
		1,    /* initialized */
		NULL, /* private */
		NULL, /* startElementNsSAX2Func */
		NULL, /* endElementNsSAX2Func */
		NULL  /* xmlStructuredErrorFunc */
	};

	string = g_string_new (NULL);
	doc = htmlSAXParseDoc ((xmlChar *) html, "UTF-8", &handler, string);

	if (doc) {
		xmlFreeDoc (doc);
	}

	return g_string_free (string, FALSE);
}

static void
tracker_miner_rss_finalize (GObject *object)
{
	TrackerMinerRSSPrivate *priv;

	priv = TRACKER_MINER_RSS_GET_PRIVATE (object);

	priv->stopped = TRUE;
	g_free (priv->last_status);
	g_object_unref (priv->pool);

	g_dbus_connection_signal_unsubscribe (priv->connection, priv->graph_updated_id);
	g_object_unref (priv->connection);

	g_list_foreach (priv->item_inserts, (GFunc) feed_item_insert_data_free, NULL);
	g_list_free (priv->item_inserts);

	g_hash_table_unref (priv->channel_updates);
	g_hash_table_unref (priv->channels);

	G_OBJECT_CLASS (tracker_miner_rss_parent_class)->finalize (object);
}

static gint
get_class_id (TrackerSparqlConnection *conn,
              const gchar             *class)
{
	TrackerSparqlCursor *cursor;
	GError *error = NULL;
	gchar *query;
	gint id = -1;

	query = g_strdup_printf ("select tracker:id (%s) {}", class);
	cursor = tracker_sparql_connection_query (conn, query, NULL, &error);
	g_free (query);

	if (error) {
		g_critical ("Could not get class ID for '%s': %s\n",
		            class, error->message);
		g_error_free (error);
		return -1;
	}

	if (tracker_sparql_cursor_next (cursor, NULL, NULL))
		id = tracker_sparql_cursor_get_integer (cursor, 0);
	else
		g_critical ("'%s' didn't resolve to a known class ID", class);

	g_object_unref (cursor);

	return id;
}

static gboolean
miner_connected (TrackerMinerOnline *miner,
		 TrackerNetworkType  network)
{
	return (network == TRACKER_NETWORK_TYPE_LAN);
}

static void
tracker_miner_rss_class_init (TrackerMinerRSSClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	TrackerMinerClass *miner_class = TRACKER_MINER_CLASS (klass);
	TrackerMinerOnlineClass *miner_online_class = TRACKER_MINER_ONLINE_CLASS (klass);

	object_class->finalize = tracker_miner_rss_finalize;

	miner_class->started = miner_started;
	miner_class->stopped = miner_stopped;
	miner_class->paused  = miner_paused;
	miner_class->resumed = miner_resumed;

	miner_online_class->connected = miner_connected;

	g_type_class_add_private (object_class, sizeof (TrackerMinerRSSPrivate));
}

static void
tracker_miner_rss_init (TrackerMinerRSS *object)
{
	GError *error = NULL;
	TrackerMinerRSSPrivate *priv;

	g_message ("Initializing...");

	priv = TRACKER_MINER_RSS_GET_PRIVATE (object);

	priv->connection = g_bus_get_sync (TRACKER_IPC_BUS, NULL, &error);

	if (!priv->connection) {
		g_critical ("Could not connect to the D-Bus session bus, %s",
			    error ? error->message : "no error given.");
		g_error_free (error);
		return;
	}

	/* Key object reference is cleaned up in value destroy func */
	priv->channel_updates = g_hash_table_new_full (g_direct_hash,
	                                               g_direct_equal,
	                                               NULL,
	                                               (GDestroyNotify) feed_channel_update_data_free);
	priv->channels = g_hash_table_new_full (NULL, NULL, NULL,
	                                        (GDestroyNotify) g_object_unref);

	priv->pool = grss_feeds_pool_new ();
	g_signal_connect (priv->pool, "feed-fetching", G_CALLBACK (feed_fetching_cb), object);
	g_signal_connect (priv->pool, "feed-ready", G_CALLBACK (feed_ready_cb), object);
	priv->now_fetching = 0;

	g_message ("Listening for GraphUpdated changes on D-Bus interface...");
	g_message ("  arg0:'%s'", TRACKER_PREFIX_MFO "FeedChannel");

	priv->graph_updated_id =
		g_dbus_connection_signal_subscribe  (priv->connection,
		                                     "org.freedesktop.Tracker1",
		                                     "org.freedesktop.Tracker1.Resources",
		                                     "GraphUpdated",
		                                     "/org/freedesktop/Tracker1/Resources",
		                                     TRACKER_PREFIX_MFO "FeedChannel",
		                                     G_DBUS_SIGNAL_FLAGS_NONE,
		                                     graph_updated_cb,
		                                     object,
		                                     NULL);
}

static void
delete_unbound_messages (TrackerMinerRSS *miner)
{
	tracker_sparql_connection_update_async (tracker_miner_get_connection (TRACKER_MINER (miner)),
	                                        "DELETE { ?msg a rdfs:Resource }"
	                                        "WHERE  { ?msg a mfo:FeedMessage ."
	                                        "              FILTER(!BOUND(nmo:communicationChannel(?msg)))"
	                                        "}",
	                                        G_PRIORITY_DEFAULT,
	                                        NULL, NULL, NULL);
}

static void
delete_message_channels_cb (GObject      *source_object,
                            GAsyncResult *res,
                            gpointer      user_data)
{
	TrackerMinerRSS *miner = user_data;
	TrackerSparqlConnection *connection;
	GError *error = NULL;

	connection = TRACKER_SPARQL_CONNECTION (source_object);
	tracker_sparql_connection_update_finish (connection, res, &error);

	if (error != NULL) {
		g_message ("Could not delete message channels: %s", error->message);
		g_error_free (error);
		return;
	}

	delete_unbound_messages (miner);
}

static void
delete_message_channels (TrackerMinerRSS *miner,
                         GArray          *channel_ids)
{
	TrackerMinerRSSPrivate *priv;
	GString *query, *ids_str;
	GrssFeedChannel *channel;
	gint i, id;

	priv = TRACKER_MINER_RSS_GET_PRIVATE (miner);
	ids_str = g_string_new (NULL);
	query = g_string_new (NULL);

	for (i = 0; i < channel_ids->len; i++) {
		id = g_array_index (channel_ids, gint, i);
		if (i != 0)
			g_string_append (ids_str, ",");
		g_string_append_printf (ids_str, "%d", id);

		channel = g_hash_table_lookup (priv->channels,
					       GINT_TO_POINTER (id));

		if (channel) {
			g_hash_table_remove (priv->channel_updates, channel);
			g_hash_table_remove (priv->channels, GINT_TO_POINTER (id));
		}
	}

	g_string_append_printf (query,
	                        "DELETE { ?msg nmo:communicationChannel ?chan }"
	                        "WHERE  { ?msg a mfo:FeedMessage;"
	                        "              nmo:communicationChannel ?chan ."
	                        "              FILTER (tracker:id(?chan) IN (%s))"
	                        "}", ids_str->str);

	tracker_sparql_connection_update_async (tracker_miner_get_connection (TRACKER_MINER (miner)),
	                                        query->str, G_PRIORITY_DEFAULT,
	                                        NULL,
	                                        delete_message_channels_cb,
	                                        miner);
	g_string_free (ids_str, TRUE);
	g_string_free (query, TRUE);
}

static void
handle_deletes (TrackerMinerRSS *miner,
                GVariantIter    *iter)
{
	GArray *deleted = g_array_new (FALSE, FALSE, sizeof (gint));
	gint graph, subject, pred, object;
	TrackerMinerRSSPrivate *priv;

	priv = TRACKER_MINER_RSS_GET_PRIVATE (miner);

	while (g_variant_iter_next (iter, "(iiii)",
	                            &graph, &subject, &pred, &object)) {
		if (pred == priv->rdf_type_id &&
		    object == priv->mfo_feed_channel_id) {
			g_array_append_val (deleted, subject);
		}
	}

	if (deleted->len > 0)
		delete_message_channels (miner, deleted);

	g_array_free (deleted, TRUE);
}

static void
handle_updates (TrackerMinerRSS *miner,
                GVariantIter    *iter)
{
	GArray *updated = g_array_new (FALSE, FALSE, sizeof (gint));
	gint graph, subject, pred, object;
	TrackerMinerRSSPrivate *priv;

	priv = TRACKER_MINER_RSS_GET_PRIVATE (miner);

	while (g_variant_iter_next (iter, "(iiii)",
	                            &graph, &subject, &pred, &object)) {
		if (pred == priv->rdf_type_id &&
		    object == priv->mfo_feed_channel_id) {
			g_array_append_val (updated, subject);
		}
	}

	if (updated->len > 0)
		retrieve_and_schedule_feeds (miner, updated);

	g_array_free (updated, TRUE);
}

static void
graph_updated_cb (GDBusConnection *connection,
                  const gchar     *sender_name,
                  const gchar     *object_path,
                  const gchar     *interface_name,
                  const gchar     *signal_name,
                  GVariant        *parameters,
                  gpointer         user_data)
{
	TrackerMinerRSS *miner = TRACKER_MINER_RSS (user_data);
	GVariantIter *deletes, *updates;

	g_variant_get (parameters, "(&sa(iiii)a(iiii))", NULL, &deletes, &updates);
	handle_deletes (miner, deletes);
	handle_updates (miner, updates);
	g_variant_iter_free (deletes);
	g_variant_iter_free (updates);
}

static FeedChannelUpdateData *
feed_channel_update_data_new (TrackerMinerRSS *miner,
                              GrssFeedChannel     *channel)
{
	FeedChannelUpdateData *fcud;

	fcud = g_slice_new0 (FeedChannelUpdateData);
	fcud->miner = g_object_ref (miner);
	fcud->channel = g_object_ref (channel);
	fcud->timeout_id = g_timeout_add_seconds (2, feed_channel_changed_timeout_cb, fcud);
	fcud->cancellable = g_cancellable_new ();

	return fcud;
}

static void
feed_channel_update_data_free (FeedChannelUpdateData *fcud)
{
	if (!fcud) {
		return;
	}

	if (fcud->cancellable) {
		g_cancellable_cancel (fcud->cancellable);
		g_object_unref (fcud->cancellable);
	}

	if (fcud->timeout_id) {
		g_source_remove (fcud->timeout_id);
	}

	if (fcud->channel) {
		g_object_unref (fcud->channel);
	}

	if (fcud->miner) {
		g_object_unref (fcud->miner);
	}

	g_slice_free (FeedChannelUpdateData, fcud);
}

static FeedItemInsertData *
feed_item_insert_data_new (TrackerMinerRSS *miner,
                           GrssFeedItem    *item)
{
	FeedItemInsertData *fiid;

	fiid = g_slice_new0 (FeedItemInsertData);
	fiid->miner = g_object_ref (miner);
	fiid->item = g_object_ref (item);
	fiid->cancellable = g_cancellable_new ();

	return fiid;
}

static void
feed_item_insert_data_free (FeedItemInsertData *fiid)
{
	if (!fiid) {
		return;
	}

	if (fiid->cancellable) {
		g_cancellable_cancel (fiid->cancellable);
		g_object_unref (fiid->cancellable);
	}

	if (fiid->item) {
		g_object_unref (fiid->item);
	}

	if (fiid->miner) {
		g_object_unref (fiid->miner);
	}

	g_slice_free (FeedItemInsertData, fiid);
}

static void
feed_channel_change_updated_time_cb (GObject      *source,
                                     GAsyncResult *result,
                                     gpointer      user_data)
{
	TrackerMinerRSSPrivate *priv;
	FeedChannelUpdateData *fcud;
	GError *error = NULL;

	fcud = user_data;
	priv = TRACKER_MINER_RSS_GET_PRIVATE (fcud->miner);

	tracker_sparql_connection_update_finish (TRACKER_SPARQL_CONNECTION (source), result, &error);
	if (error != NULL) {
		g_critical ("Could not change feed channel updated time, %s", error->message);
		g_error_free (error);
	}

	/* This will clean up the fcud data too */
	g_hash_table_remove (priv->channel_updates, fcud->channel);
}

static gboolean
feed_channel_changed_timeout_cb (gpointer user_data)
{
	TrackerSparqlBuilder *sparql;
	FeedChannelUpdateData *fcud;
	gchar *uri;
	time_t now;

	fcud = user_data;
	fcud->timeout_id = 0;

	now = time (NULL);
	uri = g_object_get_data (G_OBJECT (fcud->channel), "subject");

	g_message ("Updating mfo:updatedTime for channel '%s'", grss_feed_channel_get_title (fcud->channel));

	/* I hope there will be soon a SPARQL command to just update a
	 * value instead to delete and re-insert it
	 */

	sparql = tracker_sparql_builder_new_update ();
	tracker_sparql_builder_delete_open (sparql, NULL);
	tracker_sparql_builder_subject_iri (sparql, uri);
	tracker_sparql_builder_predicate (sparql, "mfo:updatedTime");
	tracker_sparql_builder_object_variable (sparql, "unknown");
	tracker_sparql_builder_delete_close (sparql);
	tracker_sparql_builder_where_open (sparql);
	tracker_sparql_builder_subject_iri (sparql, uri);
	tracker_sparql_builder_predicate (sparql, "mfo:updatedTime");
	tracker_sparql_builder_object_variable (sparql, "unknown");
	tracker_sparql_builder_where_close (sparql);

	tracker_sparql_builder_insert_open (sparql, NULL);
	tracker_sparql_builder_subject_iri (sparql, uri);
	tracker_sparql_builder_predicate (sparql, "mfo:updatedTime");
	tracker_sparql_builder_object_date (sparql, &now);
	tracker_sparql_builder_insert_close (sparql);

	tracker_sparql_connection_update_async (tracker_miner_get_connection (TRACKER_MINER (fcud->miner)),
	                                        tracker_sparql_builder_get_result (sparql),
	                                        G_PRIORITY_DEFAULT,
	                                        fcud->cancellable,
	                                        feed_channel_change_updated_time_cb,
	                                        fcud);
	g_object_unref (sparql);

	return FALSE;
}

static void
feed_channel_change_updated_time (FeedItemInsertData *fiid)
{
	TrackerMinerRSSPrivate *priv;
	GrssFeedChannel *channel;
	FeedChannelUpdateData *fcud;

	priv = TRACKER_MINER_RSS_GET_PRIVATE (fiid->miner);

	/* Check we don't already have an update request for this channel */
	channel = grss_feed_item_get_parent (fiid->item);

	fcud = g_hash_table_lookup (priv->channel_updates, channel);
	if (fcud) {
		/* We already had an update for this channel in
		 * progress, so we just reset the timeout.
		 */
		g_source_remove (fcud->timeout_id);
		fcud->timeout_id = g_timeout_add_seconds (2,
		                                          feed_channel_changed_timeout_cb,
		                                          fcud);
	} else {
		/* This is a new update for this channel */
		fcud = feed_channel_update_data_new (fiid->miner, channel);
		g_hash_table_insert (priv->channel_updates,
		                     fcud->channel,
		                     fcud);
	}
}

static void
feed_fetching_cb (GrssFeedsPool   *pool,
                  GrssFeedChannel *channel,
                  gpointer        user_data)
{
	gint avail;
	gdouble prog;
	TrackerMinerRSS *miner;
	TrackerMinerRSSPrivate *priv;

	miner = TRACKER_MINER_RSS (user_data);
	priv = TRACKER_MINER_RSS_GET_PRIVATE (miner);
	avail = grss_feeds_pool_get_listened_num (priv->pool);

	priv->now_fetching++;

	if (priv->now_fetching > avail)
		priv->now_fetching = avail;

	g_message ("Fetching channel details, source:'%s' (in progress: %d/%d)",
	           grss_feed_channel_get_source (channel),
	           priv->now_fetching,
	           avail);

	prog = ((gdouble) priv->now_fetching) / ((gdouble) avail);
	g_object_set (miner, "progress", prog, "status", "Fetching…", NULL);
}

static void
feed_item_insert_cb (GObject      *source,
                     GAsyncResult *result,
                     gpointer      user_data)
{
	FeedItemInsertData *fiid;
	GError *error;
	const gchar *title;

	fiid = user_data;
	title = grss_feed_item_get_title (fiid->item);
	error = NULL;

	tracker_sparql_connection_update_finish (TRACKER_SPARQL_CONNECTION (source), result, &error);
	if (error != NULL) {
		g_critical ("Could not insert feed information for message titled:'%s', %s",
		            title,
		            error->message);
		g_error_free (error);
	} else {
		feed_channel_change_updated_time (fiid);
	}

	feed_item_insert_data_free (fiid);
}

static gchar *
sparql_add_website (TrackerSparqlBuilder *sparql,
                    const gchar          *uri)
{
	gchar *website_urn;

	website_urn = tracker_sparql_escape_uri_printf ("urn:website:%s", uri);

	tracker_sparql_builder_insert_silent_open (sparql, NULL);
	tracker_sparql_builder_subject_iri (sparql, website_urn);
	tracker_sparql_builder_predicate (sparql, "a");
	tracker_sparql_builder_object (sparql, "nie:DataObject");
	tracker_sparql_builder_object (sparql, "nfo:Website");

	tracker_sparql_builder_predicate (sparql, "nie:url");
	tracker_sparql_builder_object_unvalidated (sparql, uri);

	tracker_sparql_builder_insert_close (sparql);

	return website_urn;
}

static void
sparql_add_contact (TrackerSparqlBuilder *sparql,
                    const gchar          *alias,
                    GrssPerson           *contact,
                    const gchar          *website_urn)
{
	const gchar *name = grss_person_get_name (contact);
	const gchar *email = grss_person_get_email (contact);

	tracker_sparql_builder_subject (sparql, alias);
	tracker_sparql_builder_predicate (sparql, "a");
	tracker_sparql_builder_object (sparql, "nco:Contact");

	tracker_sparql_builder_predicate (sparql, "nco:fullname");
	tracker_sparql_builder_object_unvalidated (sparql, name);

	if (email != NULL) {
		tracker_sparql_builder_predicate (sparql, "nco:hasEmailAddress");

		tracker_sparql_builder_object_blank_open (sparql);

		tracker_sparql_builder_predicate (sparql, "a");
		tracker_sparql_builder_object (sparql, "nco:EmailAddress");

		tracker_sparql_builder_predicate (sparql, "nco:emailAddress");
		tracker_sparql_builder_object_unvalidated (sparql, email);
		tracker_sparql_builder_object_blank_close (sparql);
	}

	if (website_urn) {
		tracker_sparql_builder_predicate (sparql, "nco:websiteUrl");
		tracker_sparql_builder_object_iri (sparql, website_urn);
	}
}

static TrackerSparqlBuilder *
feed_message_create_insert_builder (TrackerMinerRSS    *miner,
                                    GrssFeedItem       *item,
                                    const gchar        *item_urn)
{
	time_t t;
	gchar *uri;
	const gchar *url;
	GrssPerson *author;
	gdouble latitude;
	gdouble longitude;
	const gchar *tmp_string;
	TrackerSparqlBuilder *sparql;
	GrssFeedChannel *channel;
	gboolean has_geolocation;
	const GList *contributors;
	const GList *list, *l;
	GList *contrib_aliases = NULL;
	gchar *website_urn = NULL;
	GHashTable *contributor_websites;
	gboolean is_iri = FALSE;

	if (!item_urn) {
		item_urn = "_:message";
	} else {
		is_iri = TRUE;
	}

	url = get_message_url (item);
	g_message ("Inserting feed item for '%s'", url);

	contributor_websites = g_hash_table_new_full (NULL, NULL, NULL,
	                                              (GDestroyNotify) g_free);
	sparql = tracker_sparql_builder_new_update ();
	author = grss_feed_item_get_author (item);
	contributors = grss_feed_item_get_contributors (item);
	channel = grss_feed_item_get_parent (item);

	for (l = contributors; l; l = l->next) {
		const gchar *person_url;
		gchar *urn;

		person_url = grss_person_get_uri (l->data);

		if (!person_url)
			continue;

		urn = sparql_add_website (sparql, person_url);
		g_hash_table_insert (contributor_websites, l->data, urn);
	}

	if (author && grss_person_get_uri (author)) {
		website_urn = sparql_add_website (sparql, grss_person_get_uri (author));
	}

	has_geolocation = grss_feed_item_get_geo_point (item, &latitude, &longitude);
	tracker_sparql_builder_insert_open (sparql, NULL);

	if (has_geolocation) {
		g_message ("  Geolocation, using longitude:%f, latitude:%f",
		           longitude, latitude);

		tracker_sparql_builder_subject (sparql, "_:location");
		tracker_sparql_builder_predicate (sparql, "a");
		tracker_sparql_builder_object (sparql, "slo:GeoLocation");

		tracker_sparql_builder_predicate (sparql, "slo:latitude");
		tracker_sparql_builder_object_double (sparql, latitude);
		tracker_sparql_builder_predicate (sparql, "slo:longitude");
		tracker_sparql_builder_object_double (sparql, longitude);
	}

	if (author != NULL) {
		g_message ("  Author:'%s'", grss_person_get_name (author));
		sparql_add_contact (sparql, "_:author", author, website_urn);
	}

	for (l = contributors; l; l = l->next) {
		gchar *subject;
		gint i = 0;

		g_debug ("  Contributor:'%s'", grss_person_get_name (l->data));

		subject = g_strdup_printf ("_:contrib%d", i++);
		contrib_aliases = g_list_prepend (contrib_aliases, subject);
		sparql_add_contact (sparql, subject, l->data,
		                    g_hash_table_lookup (contributor_websites, l->data));
		g_free (subject);
	}

	if (is_iri) {
		tracker_sparql_builder_subject_iri (sparql, item_urn);
	} else {
		tracker_sparql_builder_subject (sparql, item_urn);
	}
	tracker_sparql_builder_predicate (sparql, "a");
	tracker_sparql_builder_object (sparql, "mfo:FeedMessage");
	tracker_sparql_builder_predicate (sparql, "a");
	tracker_sparql_builder_object (sparql, "nfo:RemoteDataObject");

	if (has_geolocation == TRUE) {
		tracker_sparql_builder_predicate (sparql, "slo:location");
		tracker_sparql_builder_object (sparql, "_:location");
	}

	tmp_string = grss_feed_item_get_title (item);
	if (tmp_string != NULL) {
		g_message ("  Title:'%s'", tmp_string);

		tracker_sparql_builder_predicate (sparql, "nie:title");
		tracker_sparql_builder_object_unvalidated (sparql, tmp_string);
	}

	if (author != NULL) {
		tracker_sparql_builder_predicate (sparql, "nco:creator");
		tracker_sparql_builder_object (sparql, "_:author");
	}

	for (l = contrib_aliases; l; l = l->next) {
		tracker_sparql_builder_predicate (sparql, "nco:contributor");
		tracker_sparql_builder_object (sparql, l->data);
	}

	tmp_string = grss_feed_item_get_description (item);
	if (tmp_string != NULL) {
		gchar *plain_text;

		plain_text = parse_html_text (tmp_string);
		tracker_sparql_builder_predicate (sparql, "nie:plainTextContent");
		tracker_sparql_builder_object_unvalidated (sparql, plain_text);
		g_free (plain_text);

		tracker_sparql_builder_predicate (sparql, "nmo:htmlMessageContent");
		tracker_sparql_builder_object_unvalidated (sparql, tmp_string);
	}

	if (url != NULL) {
		tracker_sparql_builder_predicate (sparql, "nie:url");
		tracker_sparql_builder_object_unvalidated (sparql, url);
	}

	/* TODO nmo:receivedDate and mfo:downloadedTime are the same?
	 *      Ask for the MFO maintainer */

	t = time (NULL);

	tracker_sparql_builder_predicate (sparql, "nmo:receivedDate");
	tracker_sparql_builder_object_date (sparql, &t);

	tracker_sparql_builder_predicate (sparql, "mfo:downloadedTime");
	tracker_sparql_builder_object_date (sparql, &t);

	t = grss_feed_item_get_publish_time (item);
	tracker_sparql_builder_predicate (sparql, "nie:contentCreated");
	tracker_sparql_builder_object_date (sparql, &t);

	tracker_sparql_builder_predicate (sparql, "nmo:isRead");
	tracker_sparql_builder_object_boolean (sparql, FALSE);

	uri = g_object_get_data (G_OBJECT (channel), "subject");
	tracker_sparql_builder_predicate (sparql, "nmo:communicationChannel");
	tracker_sparql_builder_object_iri (sparql, uri);

	tmp_string = grss_feed_item_get_copyright (item);
	if (tmp_string) {
		tracker_sparql_builder_predicate (sparql, "nie:copyright");
		tracker_sparql_builder_object_unvalidated (sparql, tmp_string);
	}

	list = grss_feed_item_get_categories (item);
	for (l = list; l; l = l->next) {
		tracker_sparql_builder_predicate (sparql, "nie:keyword");
		tracker_sparql_builder_object_unvalidated (sparql, l->data);
	}

	tracker_sparql_builder_insert_close (sparql);

	g_list_foreach (contrib_aliases, (GFunc) g_free, NULL);
	g_list_free (contrib_aliases);

	g_free (website_urn);
	g_hash_table_destroy (contributor_websites);

	return sparql;
}

static void
feed_item_check_exists_cb (GObject      *source_object,
                           GAsyncResult *res,
                           gpointer      user_data)
{
	TrackerSparqlConnection *connection;
	FeedItemInsertData *fiid;
	TrackerSparqlCursor *cursor;
	GError *error;
	TrackerSparqlBuilder *sparql;

	fiid = user_data;
	connection = TRACKER_SPARQL_CONNECTION (source_object);
	error = NULL;
	cursor = tracker_sparql_connection_query_finish (connection, res, &error);

	if (error != NULL) {
		g_message ("Could not verify feed existance, %s", error->message);
		g_error_free (error);

		if (cursor) {
			g_object_unref (cursor);
		}

		feed_item_insert_data_free (fiid);

		return;
	}

	if (!tracker_sparql_cursor_next (cursor, NULL, NULL)) {
		g_message ("No data in query response??");

		if (cursor) {
			g_object_unref (cursor);
		}

		feed_item_insert_data_free (fiid);

		return;
	}

	if (tracker_sparql_cursor_get_boolean (cursor, 0)) {
		g_debug ("  Item already exists '%s'",
		         grss_feed_item_get_title (fiid->item));

		if (cursor) {
			g_object_unref (cursor);
		}

		feed_item_insert_data_free (fiid);

		return;
	}

	sparql = feed_message_create_insert_builder (fiid->miner, fiid->item, NULL);
	tracker_sparql_connection_update_async (connection,
	                                        tracker_sparql_builder_get_result (sparql),
	                                        G_PRIORITY_DEFAULT,
	                                        fiid->cancellable,
	                                        feed_item_insert_cb,
	                                        fiid);
	g_object_unref (sparql);
	g_object_unref (cursor);
}

static void
feed_item_check_exists (TrackerMinerRSS *miner,
                        GrssFeedItem    *item)
{
	FeedItemInsertData *fiid;
	gchar *query;
	const gchar *url;

	url = get_message_url (item);

	query = g_strdup_printf ("ASK {"
	                         "  ?message a mfo:FeedMessage ;"
	                         "             nie:url \"%s\""
	                         "}",
	                         url);

	fiid = feed_item_insert_data_new (miner, item);

	tracker_sparql_connection_query_async (tracker_miner_get_connection (TRACKER_MINER (miner)),
	                                       query,
	                                       fiid->cancellable,
	                                       feed_item_check_exists_cb,
	                                       fiid);
	g_free (query);
}

static void
update_feed_channel_info (TrackerMinerRSS *miner,
                          GrssFeedChannel *channel)
{
	const gchar *subject, *str;
	GString *update;
	gchar *escaped;
	time_t time;

	g_debug ("Updating mfo:FeedChannel for '%s'",
	         grss_feed_channel_get_title (channel));

	subject = g_object_get_data (G_OBJECT (channel), "subject");
	update = g_string_new ("INSERT OR REPLACE { ");

	str = grss_feed_channel_get_title (channel);
	if (str) {
		escaped = tracker_sparql_escape_string (str);
		g_string_append_printf (update, "<%s> nie:title \"%s\".", subject, escaped);
		g_free (escaped);
	}

	str = grss_feed_channel_get_format (channel);
	if (str) {
		escaped = tracker_sparql_escape_string (str);
		g_string_append_printf (update,
		                        "<%s> mfo:type [ a mfo:FeedType ;"
		                        " mfo:name \"%s\"].",
		                        subject, escaped);
		g_free (escaped);
	}

	str = grss_feed_channel_get_description (channel);
	if (str) {
		escaped = tracker_sparql_escape_string (str);
		g_string_append_printf (update, "<%s> nie:description \"%s\".", subject, escaped);
		g_free (escaped);
	}

	str = grss_feed_channel_get_image (channel);
	if (str) {
		g_string_append_printf (update, "<%s> mfo:image \"%s\".", subject, str);
	}

	str = grss_feed_channel_get_copyright (channel);
	if (str) {
		escaped = tracker_sparql_escape_string (str);
		g_string_append_printf (update, "<%s> nie:copyright \"%s\".", subject, escaped);
		g_free (escaped);
	}

	time = grss_feed_channel_get_publish_time (channel);

	if (time != 0) {
		escaped = tracker_date_to_string (time);
		g_string_append_printf (update, "<%s> nmo:lastMessageDate \"%s\".", subject, escaped);
		g_free (escaped);
	}

	g_string_append (update, "}");

	tracker_sparql_connection_update_async (tracker_miner_get_connection (TRACKER_MINER (miner)),
	                                        update->str, G_PRIORITY_DEFAULT,
	                                        NULL, NULL, NULL);
	g_string_free (update, TRUE);
}

static void
feed_ready_cb (GrssFeedsPool   *pool,
               GrssFeedChannel *channel,
               GList           *items,
               gpointer         user_data)
{
	TrackerMinerRSS *miner;
	TrackerMinerRSSPrivate *priv;
	GList *iter;

	miner = TRACKER_MINER_RSS (user_data);
	priv = TRACKER_MINER_RSS_GET_PRIVATE (miner);

	priv->now_fetching--;

	g_debug ("Feed fetched, %d remaining", priv->now_fetching);

	if (priv->now_fetching <= 0) {
		priv->now_fetching = 0;
		g_object_set (miner, "progress", 1.0, "status", "Idle", NULL);
	}

	if (items == NULL) {
		return;
	}

	update_feed_channel_info (miner, channel);

	g_message ("Verifying channel:'%s' is up to date",
	           grss_feed_channel_get_title (channel));

	for (iter = items; iter; iter = iter->next) {
		GrssFeedItem *item = iter->data;

		feed_item_check_exists (miner, item);
	}
}

static void
feeds_retrieve_cb (GObject      *source_object,
                   GAsyncResult *res,
                   gpointer      user_data)
{
	GList *channels;
	TrackerSparqlCursor *cursor;
	GError *error = NULL;
	TrackerMinerRSSPrivate *priv;
	GrssFeedChannel *chan;

	priv = TRACKER_MINER_RSS_GET_PRIVATE (user_data);
	cursor = tracker_sparql_connection_query_finish (TRACKER_SPARQL_CONNECTION (source_object),
	                                                 res,
	                                                 &error);

	if (error != NULL) {
		g_message ("Could not retrieve feeds, %s", error->message);
		g_error_free (error);
		if (cursor) {
			g_object_unref (cursor);
		}
		return;
	}

	while (tracker_sparql_cursor_next (cursor, NULL, NULL)) {
		const gchar *source;
		const gchar *title;
		const gchar *interval;
		const gchar *subject;
		gint mins, id;

		source = tracker_sparql_cursor_get_string (cursor, 0, NULL);
		title = tracker_sparql_cursor_get_string (cursor, 1, NULL);
		interval = tracker_sparql_cursor_get_string (cursor, 2, NULL);
		subject = tracker_sparql_cursor_get_string (cursor, 3, NULL);
		id = tracker_sparql_cursor_get_integer (cursor, 4);

		g_debug ("Indexing channel '%s'", source);

		if (g_hash_table_lookup (priv->channels, GINT_TO_POINTER (id)))
			continue;

		chan = grss_feed_channel_new ();
		g_object_set_data_full (G_OBJECT (chan),
		                        "subject",
		                        g_strdup (subject),
		                        g_free);
		grss_feed_channel_set_source (chan, g_strdup (source));

		/* TODO How to manage feeds with an update mfo:updateInterval == 0 ?
		 * Here the interval is forced to be at least 1 minute, but perhaps those
		 * elements are to be considered "disabled"
		 */
		mins = strtoull (interval, NULL, 10);
		if (mins <= 0)
			mins = 1;
		grss_feed_channel_set_update_interval (chan, mins);

		g_message ("  '%s' (%s) - update interval of %s minutes",
		           title,
		           source,
		           interval);

		g_hash_table_insert (priv->channels, GINT_TO_POINTER (id), chan);
	}

	if (g_hash_table_size (priv->channels) == 0) {
		g_message ("No feeds set up, nothing more to do");
	}

	channels = g_hash_table_get_values (priv->channels);
	grss_feeds_pool_listen (priv->pool, channels);
	g_list_free (channels);

	g_object_unref (cursor);

	if (g_hash_table_size (priv->channels) == 0) {
		g_object_set (user_data, "progress", 1.0, "status", "Idle", NULL);
	}
}

static void
retrieve_and_schedule_feeds (TrackerMinerRSS *miner,
                             GArray          *channel_ids)
{
	GString *sparql;
	gint i, id;

	g_message ("Retrieving and scheduling feeds...");

	sparql = g_string_new ("SELECT ?url nie:title(?urn) ?interval ?urn tracker:id(?urn)"
	                       "WHERE {"
	                       "  ?urn a mfo:FeedChannel ; "
	                       "         mfo:feedSettings ?settings ; "
	                       "         nie:url ?url . "
	                       "  ?settings mfo:updateInterval ?interval ");

	if (channel_ids && channel_ids->len > 0) {
		g_string_append (sparql, ". FILTER (tracker:id(?urn) IN (");

		for (i = 0; i < channel_ids->len; i++) {
			id = g_array_index (channel_ids, gint, i);
			if (i != 0)
				g_string_append (sparql, ",");
			g_string_append_printf (sparql, "%d", id);
		}

		g_string_append (sparql, "))");
	}

	g_string_append_printf (sparql, "}");

	tracker_sparql_connection_query_async (tracker_miner_get_connection (TRACKER_MINER (miner)),
	                                       sparql->str,
	                                       NULL,
	                                       feeds_retrieve_cb,
	                                       miner);
	g_string_free (sparql, TRUE);
}

static const gchar *
get_message_url (GrssFeedItem *item)
{
	const gchar *url;

	grss_feed_item_get_real_source (item, &url, NULL);
	if (url == NULL)
		url = grss_feed_item_get_source (item);
	return url;
}

static void
miner_started (TrackerMiner *miner)
{
	TrackerMinerRSSPrivate *priv;
	TrackerSparqlConnection *conn;

	g_object_set (miner, "progress", 0.0, "status", "Initializing", NULL);

	priv = TRACKER_MINER_RSS_GET_PRIVATE (miner);
	conn = tracker_miner_get_connection (miner);
	priv->rdf_type_id = get_class_id (conn, "rdf:type");
	priv->mfo_feed_channel_id = get_class_id (conn, "mfo:FeedChannel");
	retrieve_and_schedule_feeds (TRACKER_MINER_RSS (miner), NULL);
	grss_feeds_pool_switch (priv->pool, TRUE);
}

static void
miner_stopped (TrackerMiner *miner)
{
	TrackerMinerRSSPrivate *priv;

	priv = TRACKER_MINER_RSS_GET_PRIVATE (miner);
	grss_feeds_pool_switch (priv->pool, FALSE);
	g_object_set (miner, "progress", 1.0, "status", "Idle", NULL);
}

static void
miner_paused (TrackerMiner *miner)
{
	TrackerMinerRSSPrivate *priv;

	priv = TRACKER_MINER_RSS_GET_PRIVATE (miner);
	grss_feeds_pool_switch (priv->pool, FALSE);

	/* Save last status */
	g_free (priv->last_status);
	g_object_get (miner, "status", &priv->last_status, NULL);

	/* Set paused */
	g_object_set (miner, "status", "Paused", NULL);
}

static void
miner_resumed (TrackerMiner *miner)
{
	TrackerMinerRSSPrivate *priv;

	priv = TRACKER_MINER_RSS_GET_PRIVATE (miner);
	grss_feeds_pool_switch (priv->pool, TRUE);

	/* Resume */
	g_object_set (miner, "status", priv->last_status ? priv->last_status : "Idle", NULL);
}

TrackerMinerRSS *
tracker_miner_rss_new (GError **error)
{
	return g_initable_new (TRACKER_TYPE_MINER_RSS,
	                       NULL,
	                       error,
	                       "name", "RSS",
	                       NULL);
}
