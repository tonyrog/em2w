/*
 * em2w_deliver
 *
 *   read a mail message (from procmail) extract the header
 *   locate the Subject: line and extract the
 *   em2w parameters
 *
 *   Syntax: Subject: [command] site-name/page-name/item-name
 *
 *   where site-name is a the web site name
 *         page-name is the name of the page on the site
 *         item-name is the include item on that page
 *
 *         command is add,delete,replace
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#define SPACE(x) (((x) == ' ') || ((x) == '\t'))
#define NL(x)    (((x) == '\n')||((x) == '\r'))

// strndup is kinda standard nowadays
#if 0
char* strndup(char* ptr, int len)
{
    char* copy;

    if ((len == 0) || (ptr == NULL))
	return NULL;
    if ((copy = (char*) malloc(len + 1)) == NULL)
	return NULL;
    memcpy(copy, ptr, len);
    copy[len] = '\0';
    return copy;
}
#endif

static char* verify_cmd = "/home/em2w/bin/smime_email";

int main(int argc, char** argv)
{
    char fname[] = "/home/em2w/incoming/messageXXXXXX";
    char buffer[2048];
    int fd;
    int subject = 0;
    int content_type = 0;
    int eoh = 0;
    char* command = NULL;
    char* object = NULL;
    char* sender = NULL;
    char* ssender = NULL;
    char* verified = "false";
    int verify = 0;

    if ((fd = mkstemp(fname)) < 0) {
	perror("mkstemp");
	exit(1);
    }

    while(fgets(buffer, sizeof(buffer), stdin) != NULL) {
	int len = strlen(buffer);

	if (!sender && strncasecmp("FROM ", buffer, 5) == 0) {
	    char* ptr = buffer + 5;
	    char* cptr;

	    while(SPACE(*ptr)) ptr++;
	    cptr = ptr;
	    while(*ptr && !NL(*ptr) && !SPACE(*ptr)) ptr++;
	    sender = strndup(cptr, ptr - cptr);
	}
	else if (!subject && strncasecmp("SUBJECT:", buffer, 8) == 0) {
	    char* ptr = buffer + 8;
	    char* cptr;
	    
	    subject = 1;

	    while(SPACE(*ptr)) ptr++;
	    cptr = ptr;
	    while(*ptr && !NL(*ptr) && !SPACE(*ptr)) ptr++;
	    command = strndup(cptr, ptr - cptr);
      
	    while(SPACE(*ptr)) ptr++;
	    cptr = ptr;
	    while(*ptr && !NL(*ptr) && !SPACE(*ptr)) ptr++;
	    object = strndup(cptr, ptr - cptr);
	}
	else if (!content_type && 
		 strncasecmp("CONTENT-TYPE:", buffer, 13) == 0) {
	    content_type = 1;
	    if (strstr(buffer, "application/x-pkcs7-signature") != NULL)
		verify = 1;
	}
	else if (!eoh && ((buffer[0] == '\n') ||
			  ((buffer[0] == '\r') && buffer[1] == '\n'))) {
	    subject = 1;
	    eoh = 1;
	}
	write(fd, buffer, len);
	write(1, buffer, len);
    }
    close(fd);

    /* extract and verify sender */
    if (verify && (sender != NULL)) {
	char pcmd[sizeof(fname) + sizeof(verify_cmd) + 2];
	FILE* f;

	strcpy(pcmd, verify_cmd);
	strcat(pcmd, " ");
	strcat(pcmd, fname);

	if ((f = popen(pcmd, "r")) != NULL) {
	    if (fgets(buffer, sizeof(buffer), f) != NULL) {
		char* ptr = buffer;
		char* cptr;
		int len = 0;

		while(SPACE(*ptr)) ptr++;
		cptr = ptr;
		while(*ptr && !NL(*ptr) && !SPACE(*ptr)) ptr++;
		ssender = strndup(cptr, ptr - cptr);
		if (strcasecmp(sender, ssender) == 0)
		    verified = "true";
	    }
	    pclose(f);
	}
    }

    if ((command != NULL) && (object != NULL)) {
	char cmd[4096];
	int n;

	if (sender == NULL)
	    sender = "anonymous";
	if (ssender == NULL)
	    ssender = "anonymous";

	n = snprintf(cmd, sizeof(cmd),
		     "/usr/local/bin/jerl -noshell "
		     "-pa /home/em2w/em2w/ebin "
		     "-s em2w command %s %s %s %s %s",
		     command, object, fname, sender, verified);
	printf("Command: %s\n", cmd);
	if ((n > 0) && (n < sizeof(cmd)))
	    system(cmd);
    }

    /* unlink(fname); */

    exit(0);
}
