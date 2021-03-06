/*
 * Copyright (C) 2009, Nokia <ivan.frade@nokia.com>
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

#include <string.h>

#include <glib.h>
#include <glib/gstdio.h>
#include <gio/gio.h>

#include <libtracker-data/tracker-data.h>
#include <libtracker-sparql/tracker-sparql.h>

typedef struct _TestInfo TestInfo;

struct _TestInfo {
	const gchar *test_name;
	gint number_of_queries;
};

const TestInfo tests[] = {
	{ "fts3aa", 2 },
	{ "fts3ae", 1 },
	{ "prefix/fts3prefix", 3 },
	{ "limits/fts3limits", 4 },
	{ NULL }
};

static gchar *datadir = NULL;

static void
test_sparql_query (gconstpointer test_data)
{
	TrackerDBCursor *cursor;
	const TestInfo *test_info;
	GError *error;
	GString *test_results;
	gchar *update, *update_filename;
	gchar *query, *query_filename;
	gchar *results, *results_filename;
	gchar *prefix, *test_prefix;
	GFile *ontology, *data_location;
	TrackerDataManager *manager;
	TrackerData *data;
	gint i;

	error = NULL;
	test_info = test_data;

	/* initialization */
	prefix = g_build_path (G_DIR_SEPARATOR_S, TOP_SRCDIR, "tests", "libtracker-fts", NULL);
	test_prefix = g_build_filename (prefix, test_info->test_name, NULL);
	ontology = g_file_new_for_path (prefix);
	g_free (prefix);

	data_location = g_file_new_for_path (datadir);

	tracker_db_journal_set_rotating (FALSE, G_MAXSIZE, NULL);
	manager = tracker_data_manager_new (TRACKER_DB_MANAGER_FORCE_REINDEX,
	                                    data_location, data_location, ontology,
	                                    FALSE, FALSE, 100, 100);
	g_initable_init (G_INITABLE (manager), NULL, &error);
	g_assert_no_error (error);

	data = tracker_data_manager_get_data (manager);
	g_object_unref (ontology);

	/* load data / perform updates */

	update_filename = g_strconcat (test_prefix, "-data.rq", NULL);
	g_file_get_contents (update_filename, &update, NULL, &error);
	g_assert_no_error (error);

	tracker_data_update_sparql (data, update, &error);
	g_assert_no_error (error);

	g_free (update_filename);
	g_free (update);

	/* perform queries */

	for (i = 1; i <= test_info->number_of_queries; i++) {
		query_filename = g_strdup_printf ("%s-%d.rq", test_prefix, i);
		g_file_get_contents (query_filename, &query, NULL, &error);
		g_assert_no_error (error);

		results_filename = g_strdup_printf ("%s-%d.out", test_prefix, i);
		g_file_get_contents (results_filename, &results, NULL, &error);
		g_assert_no_error (error);

		cursor = tracker_data_query_sparql_cursor (manager, query, &error);
		g_assert_no_error (error);

		/* compare results with reference output */

		test_results = g_string_new ("");

		if (cursor) {
			gint col;

			while (tracker_db_cursor_iter_next (cursor, NULL, &error)) {
				for (col = 0; col < tracker_db_cursor_get_n_columns (cursor); col++) {
					const gchar *str;

					if (col > 0) {
						g_string_append (test_results, "\t");
					}

					str = tracker_db_cursor_get_string (cursor, col, NULL);
					if (str != NULL) {
						/* bound variable */
						g_string_append_printf (test_results, "\"%s\"", str);
					}
				}

				g_string_append (test_results, "\n");
			}

			g_object_unref (cursor);
		}

		if (strcmp (results, test_results->str)) {
			/* print result difference */
			gchar *quoted_results;
			gchar *command_line;
			gchar *quoted_command_line;
			gchar *shell;
			gchar *diff;

			quoted_results = g_shell_quote (test_results->str);
			command_line = g_strdup_printf ("echo -n %s | diff -u %s -", quoted_results, results_filename);
			quoted_command_line = g_shell_quote (command_line);
			shell = g_strdup_printf ("sh -c %s", quoted_command_line);
			g_spawn_command_line_sync (shell, &diff, NULL, NULL, &error);
			g_assert_no_error (error);

			g_error ("%s", diff);

			g_free (quoted_results);
			g_free (command_line);
			g_free (quoted_command_line);
			g_free (shell);
			g_free (diff);
		}

		/* cleanup */

		g_free (query_filename);
		g_free (query);
		g_free (results_filename);
		g_free (results);
		g_string_free (test_results, TRUE);
	}

	g_free (test_prefix);
	g_object_unref (data_location);
	g_object_unref (manager);
}

int
main (int argc, char **argv)
{
	gint result;
	gint i;
	gchar *current_dir, *path;

	g_test_init (&argc, &argv, NULL);

	current_dir = g_get_current_dir ();
	datadir = g_build_filename (current_dir, "tracker", NULL);
	g_free (current_dir);

	g_setenv ("TRACKER_LANGUAGE_STOP_WORDS_DIR", current_dir, TRUE);

	/* add test cases */
	for (i = 0; tests[i].test_name; i++) {
		gchar *testpath;

		testpath = g_strconcat ("/libtracker-fts/", tests[i].test_name, NULL);
		g_test_add_data_func (testpath, &tests[i], test_sparql_query);
		g_free (testpath);
	}

	/* run tests */
	result = g_test_run ();

	/* clean up */
	g_print ("Removing temporary data\n");
	g_spawn_command_line_sync ("rm -R tracker/", NULL, NULL, NULL, NULL);

	path = g_build_filename (TOP_BUILDDIR, "tests", "libtracker-fts", "dconf", "user", NULL);
	g_unlink (path);
	g_free (path);
	g_free (datadir);

	return result;
}
