/* fold -- wrap each input line to fit in specified width.
   Copyright (C) 1991-2018 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* Written by David MacKenzie, djm@gnu.ai.mit.edu. */
/* This version was patched in 2018 by Chris Warburton (chriswarbo@gmail.com) */

#include <stdio.h>
#include <sys/types.h>

#include "error.h"

#define TAB_WIDTH 8

/* The official name of this program (e.g., no 'g' prefix).  */
#define PROGRAM_NAME "fold"

#define AUTHORS proper_name ("David MacKenzie")

#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1
#define false 0
#define true  1
#define bool char

/* Assuming the current column is COLUMN, return the column that
   printing C will move the cursor to.
   The first column is 0. */

static size_t
adjust_column (size_t column, char c)
{
  if (c == '\b')
    {
      if (column > 0)
        column--;
    }
  else if (c == '\r')
    column = 0;
  else if (c == '\t')
    column += TAB_WIDTH - column % TAB_WIDTH;
  else /* if (isprint (c)) */
    column++;
  return column;
}

/* Fold stdin to stdout, with maximum line length WIDTH.
   Return true if successful.  */

static bool
fold_file (size_t width)
{
  FILE *istream;
  int c;
  size_t column = 0;		/* Screen column where next char will go. */
  size_t offset_out = 0;	/* Index in 'line_out' for next char. */

  istream = stdin;

  if (istream == NULL)
    {
      fprintf (stderr, "ERROR: stdin was NULL\n");
      return false;
    }

  while ((c = getc (istream)) != EOF)
    {
      if (c == '\n')
        {
          printf ("%c", c);
          column = offset_out = 0;
          continue;
        }

    rescan:
      column = adjust_column (column, c);

      if (column > width)
        {
          /* This character would make the line too long.
             Print the line plus a newline, and make this character
             start the next line. */

          if (offset_out == 0)
            {
              offset_out++;
              printf ("%c", c);
              continue;
            }

          printf ("\n\r");
          column = offset_out = 0;
          goto rescan;
        }

      offset_out++;
      printf ("%c", c);
    }

  if (ferror (istream))
    {
      fprintf (stderr, "ERROR: file error on stdin?\n");
      return false;
    }

  return true;
}

int
main (int argc, char **argv)
{
  size_t width = 1000;
  bool ok;

  ok = fold_file (width);

  if (fclose (stdout) == EOF || fclose (stdin)   == EOF)
    return EXIT_FAILURE;

  return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}
