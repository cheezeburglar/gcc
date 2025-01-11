/* Example listener for SARIF output to a socket.
   Copyright (C) 2024 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


#define INCLUDE_SYS_SOCKETS
#define INCLUDE_SYS_UN
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "diagnostic.h"

/* FIXME.  */

static const int BACKLOG = 5;
#define BUF_SIZE 4096 /* FIXME */

int
main (int argc, char **argv)
{
  const char *p = argv[0] + strlen (argv[0]);
  while (p != argv[0] && !IS_DIR_SEPARATOR (p[-1]))
    --p;
  progname = p;
  diagnostic_initialize (global_dc, 0);

  const char * const env_var_name = "GCC_SOCKET";
  const char * const socket_path = getenv (env_var_name);
  if (!socket_path)
    fatal_error (UNKNOWN_LOCATION,
		 "environment variable %qs not set",
		 env_var_name);

  int sfd = socket (AF_UNIX, SOCK_STREAM, 0);
  if (sfd == -1)
    fatal_error (UNKNOWN_LOCATION,
		 "unable to create socket");

  struct sockaddr_un addr;
  if (strlen (socket_path) > sizeof (addr.sun_path) - 1)
    fatal_error (UNKNOWN_LOCATION,
		 "socket path too long: %qs", socket_path);

  if (remove (socket_path) == -1 && errno != ENOENT)
    fatal_error (UNKNOWN_LOCATION,
		 "socket %qs already exists", socket_path);

  memset (&addr, 0, sizeof (addr));
  addr.sun_family = AF_UNIX;
  strncpy (addr.sun_path, socket_path, sizeof (addr.sun_path) - 1);

  if (bind (sfd, (struct sockaddr *)&addr, sizeof (addr)) == -1)
    fatal_error (UNKNOWN_LOCATION,
		 "unable to bind to %qs", socket_path);

  if (listen (sfd, BACKLOG) == -1)
    fatal_error (UNKNOWN_LOCATION,
		 "unable to listen to %qs", socket_path);

  while (1)
    {
      int cfd = accept (sfd, nullptr, nullptr);
      if (cfd == -1)
	fatal_error (UNKNOWN_LOCATION,
		     "accept failed");

      ssize_t sz_read;
      char buf[BUF_SIZE];
      while ((sz_read = read (cfd, buf, BUF_SIZE)) > 0)
	if (write (1, buf, sz_read) != sz_read)
	  fatal_error (UNKNOWN_LOCATION,
		       "partial/failed write");
      if (sz_read == -1)
	fatal_error (UNKNOWN_LOCATION,
		     "error reading from client");
      if (close (cfd) == -1)
	fatal_error (UNKNOWN_LOCATION,
		     "error closing client fd");
    }
}
