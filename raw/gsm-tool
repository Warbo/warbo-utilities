/*
 * From http://lists.openmoko.org/pipermail/community/2014-May/069509.html
 *
 * This opens a commandline connected to the GSM modem of an OpenMoko FreeRunner
 * To run this on QtMoko, it needs tcc and libc6-dev installed via apt.
 *
 * You can make it runnable directly by giving it the shebang:
 *   #!/usr/bin/tcc -run
 *
 * The QtMoko software must release the modem first, which can be done like this
 *
 *   # stop QtMoko
 *   /etc/init.d/qtmoko-neo stop
 *
 *   # power-cycle the modem
 *   echo 0 > /sys/bus/platform/devices/gta02-pm-gsm.0/power_on
 *   echo 1 > /sys/bus/platform/devices/gta02-pm-gsm.0/power_on
 *
 *   ./gsm-tool /dev/ttySAC0 r115200
 *
 * Alternatively you can use socat (installable via apt), like this:
 *
 *   socat - file:/dev/ttySAC0,crtscts,crnl
 */
 
/*
 * This utility is used at Harhan Engineering Co. to connect to
 * the console ports of various targets in the lab.  Most of the latter
 * are either MicroVAXen or our own designs inspired by the VAX/MicroVAX
 * console, and this program has a few nifty features specifically
 * intended for those consoles.
 *
 * Beyond simple pass-thru of bytes in both directions, the following
 * features are provided:
 *
 *   - logging
 *   - ^P sends a break
 *   - binary upload via X command
 *   - changing console baud rate on HEC MC68302 targets (not in this version)
 *
 * This is the POSIX termios version of the program; the original version
 * was for 4.3BSD UNIX.
 *
 * Author: Michael Sokolov, Harhan Engineering Co.
 * msokolov at ivan.Harhan.ORG
 */

#include <sys/param.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/errno.h>
#include <termios.h>
#include <ctype.h>
#include <stdio.h>
#include <strings.h>

extern int errno;

int mypid;
int tfd;
FILE *tfdF;

struct termios saved_termios, my_termios, target_termios;

int kbd_eol_state = 1;
FILE *logF;

static struct speedtab {
	int	num;
	speed_t	code;
} speed_table[] = {
	{300, B300},
	{1200, B1200},
	{2400, B2400},
	{4800, B4800},
	{9600, B9600},
	{19200, B19200},
	{38400, B38400},
	{57600, B57600},
	{115200, B115200},
	{0, 0}};

main(argc, argv)
	char **argv;
{
	int zero = 0;
	struct speedtab *spd;
	int speed, rtscts = 0;
	char *cp;

	if (argc < 2 || argc > 3) {
		fprintf(stderr, "usage: %s tty [baud]\n", argv[0]);
		exit(1);
	}
	tfd = open(argv[1], O_RDWR|O_NONBLOCK);
	if (tfd < 0) {
		perror(argv[1]);
		exit(1);
	}
	tfdF = fdopen(tfd, "w");
	if (!tfdF) {
		perror("fdopen");
		exit(1);
	}
	if (argc == 3) {
		cp = argv[2];
		if (*cp == 'r') {
			cp++;
			rtscts++;
		}
		speed = atoi(cp);
	} else
		speed = 9600;
	for (spd = speed_table; spd->num; spd++)
		if (spd->num == speed)
			break;
	if (!spd->num) {
		fprintf(stderr, "%s: invalid baud rate\n", argv[2]);
		exit(1);
	}
	target_termios.c_iflag = IGNBRK;
	target_termios.c_oflag = 0;
	target_termios.c_cflag = CLOCAL|HUPCL|CREAD|CS8;
	if (rtscts)
		target_termios.c_cflag |= CRTSCTS;
	target_termios.c_lflag = 0;
	target_termios.c_cc[VMIN] = 1;
	target_termios.c_cc[VTIME] = 0;
	cfsetispeed(&target_termios, spd->code);
	cfsetospeed(&target_termios, spd->code);
	if (tcsetattr(tfd, TCSAFLUSH, &target_termios) < 0) {
		perror("tcsetattr");
		exit(1);
	}
	ioctl(tfd, FIONBIO, &zero);

	tcgetattr(0, &saved_termios);
	bcopy(&saved_termios, &my_termios, sizeof(struct termios));
	cfmakeraw(&my_termios);
	my_termios.c_cc[VMIN] = 1;
	my_termios.c_cc[VTIME] = 0;
	tcsetattr(0, TCSAFLUSH, &my_termios);

	printf("Starting session\r\n");
	mainloop();
}

mainloop()
{
	char buf[BUFSIZ];
	fd_set fds, fds1;
	register int i, cc, max;

	FD_ZERO(&fds);
	FD_SET(0, &fds);
	FD_SET(tfd, &fds);
	max = tfd + 1;
	for (;;) {
		bcopy(&fds, &fds1, sizeof(fd_set));
		i = select(max, &fds1, NULL, NULL, NULL);
		if (i < 0) {
			if (errno == EINTR)
				continue;
			tcsetattr(0, TCSAFLUSH, &saved_termios);
			perror("select");
			exit(1);
		}
		if (FD_ISSET(0, &fds1)) {
			cc = read(0, buf, sizeof buf);
			if (cc <= 0)
				quit();
			if (cc == 1 && buf[0] == 0x10) {
				sendbreak();
				continue;
			}
			if (cc == 1 && buf[0] == '!' && kbd_eol_state) {
				local_command();
				continue;
			}
			i = buf[cc-1];
			kbd_eol_state = (i == '\r') || (i == '\n');
			write(tfd, buf, cc);
		}
		if (FD_ISSET(tfd, &fds1)) {
			cc = read(tfd, buf, sizeof buf);
			if (cc <= 0) {
				tcsetattr(0, TCSAFLUSH, &saved_termios);
				fprintf(stderr, "EOF/error on target tty\n");
				exit(1);
			}
			write(1, buf, cc);
			if (logF)
				log_output(buf, cc);
		}
	}
}

log_output(buf, cc)
	char *buf;
{
	register char *cp;
	register int c, i;

	for (cp = buf, i = 0; i < cc; i++) {
		c = *cp++;
		if (c != '\r')
			putc(c, logF);
	}
}

sendbreak()
{
	tcsendbreak(tfd, 0);
}

local_command()
{
	char buf[256];
	int cc;
	register char *cp, *np;

	tcsetattr(0, TCSAFLUSH, &saved_termios);
	write(1, ":", 1);
	cc = read(0, buf, sizeof(buf) - 1);
	if (cc <= 0)
		quit(0);
	buf[cc] = '\0';

	for (cp = buf; isspace(*cp); cp++)
		;
	if (!*cp)
		goto out;
	for (np = cp; *cp && !isspace(*cp); cp++)
		;
	if (*cp)
		*cp++ = '\0';
	if (!strcasecmp(np, "log"))
		lcmd_log(cp);
	else if (!strcasecmp(np, "flush"))
		lcmd_flush();
	else if (!strcasecmp(np, "upload"))
		lcmd_upload(cp);
	else if (!strcasecmp(np, "quit"))
		quit();
	else
		fprintf(stderr, "invalid command\n");

out:	tcsetattr(0, TCSAFLUSH, &my_termios);
	return(0);
}

lcmd_log(arg)
	register char *arg;
{
	register char *cp;

	while (isspace(*arg))
		arg++;
	for (cp = arg; isgraph(*cp); cp++)
		;
	*cp = '\0';
	if (!arg[0]) {			/* check logging status */
		printf("Logging is %s\n", logF ? "on" : "off");
		return;
	} else if (!strcmp(arg, "-")) {	/* turn off */
		if (logF) {
			fclose(logF);
			logF = NULL;
			printf("Logging stopped\n");
		}
		return;
	}
	/* must be a request to turn it on */
	if (logF) {
		fprintf(stderr, "Logging is already on\n");
		return;
	}
	logF = fopen(arg, "w");
	if (logF)
		printf("Started logging to %s\n", arg);
	else
		perror(arg);
	return;
}

lcmd_flush()
{
	if (logF)
		fflush(logF);
}

lcmd_upload(arg)
	char *arg;
{
	char *filename, *loadaddr;
	register char *cp;
	FILE *df;
	struct stat st;
	u_char chksum, byte;
	char buf[256];
	int cc, chevron_count;
	register int i;

	for (cp = arg; isspace(*cp); cp++)
		;
	if (!*cp) {
toofew:		fprintf(stderr, "too few arguments\n");
		return;
	}
	for (filename = cp; *cp && !isspace(*cp); cp++)
		;
	if (!*cp)
		goto toofew;
	*cp++ = '\0';
	while (isspace(*cp))
		cp++;
	if (!*cp)
		goto toofew;
	for (loadaddr = cp; *cp && !isspace(*cp); cp++)
		;
	if (*cp)
		*cp++ = '\0';

	df = fopen(filename, "r");
	if (!df) {
		perror("filename");
		exit(1);
	}
	fstat(fileno(df), &st);
	sprintf(buf, "X %s %X\r", loadaddr, st.st_size);
	chksum = 0;
	for (cp = buf; *cp != '\r'; cp++)
		chksum += *cp;
	cp++;
	*cp++ = ~chksum + 1;
	write(tfd, buf, cp - buf);

	/* wait for the target to get ready */
	for (chevron_count = 0; chevron_count < 3; ) {
		cc = read(tfd, buf, sizeof buf);
		if (cc <= 0) {
			fprintf(stderr, "EOF/error on target tty\n");
			exit(1);
		}
		write(1, buf, cc);
		if (logF)
			log_output(buf, cc);
		for (cp = buf, i = 0; i < cc; i++) {
			if (*cp++ == '>')
				chevron_count++;
			else
				chevron_count = 0;
		}
	}

	/* pump the data */
	for (chksum = 0; (i = getc(df)) != EOF; ) {
		putc(i, tfdF);
		chksum += i;
	}
	putc(~chksum + 1, tfdF);
	fflush(tfdF);

	/* done! */
	fclose(df);
}

quit()
{
	tcsetattr(0, TCSAFLUSH, &saved_termios);
	exit(0);
}
