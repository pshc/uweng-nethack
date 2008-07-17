/* Provides IPC for status info, by Paul Collier */

#define NEED_VARARGS	/* comment line for pre-compiled headers */

#include "hack.h"

#ifdef STATUS_SOCKET

/* UNIX-specific stuff... */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

void notify_game_status(const char *message)
{
	FILE *fp;
	int sock, len;
	unsigned char message_len;
	struct sockaddr_un remote;
	
	/* Set up our socket */
	if((sock = socket(AF_UNIX, SOCK_STREAM, 0)) == -1)
		return;
	
	remote.sun_family = AF_UNIX;
	len = strlen(STATUS_SOCKET);
	if(len > 107)
		goto close_sock;
	/* Use the defined path */
	strcpy(remote.sun_path, STATUS_SOCKET);
	len += sizeof(remote.sun_family);
	if(connect(sock, (struct sockaddr *)&remote, len) == -1)
		goto close_sock;
	
	if(strlen(message) > 255)
		message_len = 255;
	else
		message_len = (unsigned char)strlen(message);
	/* Send the length first */
	write(sock, &message_len, 1);
	/* Send the message (we aren't bothering with errors at this point) */
	write(sock, message, message_len);

	/* GOTO not considered harmful for resource cleanup ;) */
close_sock:
	close(sock);
}

#endif /* STATUS_SOCKET */

