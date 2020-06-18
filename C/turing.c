#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <errno.h>

//#define ISPRINT(x) isprint(x)
#define ISPRINT(x) x >= 32 && x <= 127 ? 1 : 0
#define ISLETTER(x) isletter(x)
#define SLEN(x) strlen(x)
#define AND &&
#define OR &&
#define NOT !

/* constants */
const int NIL=0x0;              /* inits/defines nil pointer */
const int NOT_FOUND = -123456;
const int WILDCARD = 42;
const int NOTPRINT = 32;
const int MAXOUT = 100000;

/* arrays */
#define MSG(s,v) printf("[%12s]:\t %s\n", s, v);
#define HEADER() printf("%s", "------------------------------------------------------------\n");


/* ********************************************************************************************** */
/* prototypes */
/* ********************************************************************************************** */
extern char *filereader(char *);
extern char *readline(FILE *, int);
extern char readchar(void);
extern void load_tapefile(char *);
extern void print_action_table(void);
extern int nextstate(char);
extern void update_state(char, int);
extern int halting_tests(void);
extern void print_tape_state(void);
extern void msg(char *, char *, int);
/* ********************************************************************************************** */


/* ********************************************************************************************** */
// THE GLOBAL STATE OF THE TAPE
/* ********************************************************************************************** */
typedef struct TAPE_STATE_S {
    char *contents;
    char *traversal; 
    char *output; 
    int position;
    int pstate;
    int hstate;
    int direction;
} TAPE_STATE_T;

TAPE_STATE_T TAPE_STATE;
/* ********************************************************************************************** */


/* ********************************************************************************************** */
// THE CONTAINER DATA FOR EACH INDIVIDUAL ACTION TABLE ROW
/* ********************************************************************************************** */
struct STATE {
    int pstate;
    char ifchar;
    char writechar;
    int direction;
    int nstate;
    struct STATE *next;
} STATE[100];

static int NUM_STATES;
static int NUM_ITERS = 0;
static int OUTPUT_IDX = 0;
/* ********************************************************************************************** */


/* ********************************************************************************************** */
void
msg( char *type, char *msg, int code ) {
    printf( "[%12s] %d : %s\n", type, code, msg );
    if (type == "ERROR")
        exit(code);
    return;
}
/* ********************************************************************************************** */


/* ********************************************************************************************** */
/*derived from http://stackoverflow.com/questions/174531/easiest-way-to-get-files-contents-in-c */
/* ********************************************************************************************** */
void
load_tapefile(char *filename) {
    /* char *contents;
     * char *output; 
     * int position;
     * int pstate;
     * int hstate;
     * int direction;
     */

    FILE    *fp;
    int      length;
    char    *buffer;
    int      i, j;

    fp = fopen( filename, "r");
    if (fp == NULL) {
        fclose(fp);
        msg("ERROR", "CAN NOT OPEN SPECIFIED TAPE FILE", -1);
    }

    fseek (fp, 0, SEEK_END);
    length = ftell (fp);
    fseek (fp, 0, SEEK_SET);

    buffer = readline(fp, length);
    TAPE_STATE.contents = (char *) buffer;
    for (j=0; j<strlen(buffer); j++) {
        if (!ISPRINT((int) buffer[j])) {
            fclose(fp);
            msg("ERROR", "NON PRINTABLE ASCII CHARACTERS ON TAPE", -2);
        }
    }
    TAPE_STATE.output = (char *) malloc(MAXOUT);
    TAPE_STATE.traversal = (char *) malloc(MAXOUT);

    buffer = readline(fp, 80);
    sscanf(buffer, "%d", &TAPE_STATE.position);

    buffer = readline(fp, 80);
    sscanf(buffer, "%d", &TAPE_STATE.pstate);

    buffer = readline(fp, 80);
    sscanf(buffer, "%d", &TAPE_STATE.hstate);

    buffer = readline(fp, 80);
    i = 0;
    while ( buffer ) {
        if (!buffer) break;
        sscanf(buffer, "%d %c %c %d %d", &STATE[i].pstate, &STATE[i].ifchar, &STATE[i].writechar, &STATE[i].direction, &STATE[i].nstate);
        i++;
        buffer = readline(fp, 80);
    }

    NUM_STATES = i;

    fclose (fp);
    return;
}
/* ********************************************************************************************** */


/* ********************************************************************************************** */
void
print_action_table() {
    int i;
    HEADER();
    printf("ACTION TABLE\n");
    HEADER();
    printf("%s \t %s \t %s \t %s \t %s\n", "PS", "IFC", "WRTC", "DIR", "NS" );
    for (i=0; i<NUM_STATES; i++) {
        printf("%d \t %c \t %c \t %d \t %d\n", STATE[i].pstate, STATE[i].ifchar, STATE[i].writechar, STATE[i].direction, STATE[i].nstate);
    }
    HEADER();
    return;
}
/* ********************************************************************************************** */


/* ********************************************************************************************** */
char *
readline(FILE *fp, int length) {
    char *buffer;
    char *ptr;

    buffer = malloc (length);
    if (buffer) {
        ptr = fgets( buffer, length, fp );
    } 
    if (!buffer) {
        fclose(fp);
        msg("ERROR", "CAN NOT ALLOCATE MEMORY BUFFER", -3);
    }
    if (!ptr) {
        MSG("INFO", "READ FILE STREAM");
        return(NULL);
    }
    return(buffer);
}
/* ********************************************************************************************** */


/* ********************************************************************************************** */
char
readchar() {
    int pos;
    char tapechar;

    pos = TAPE_STATE.position;
    if (pos < 0 ) {
        msg( "ERROR", "TAPE HEAD OUTSIDE BOUNDARY", -4);
    }

    if (pos >= strlen(TAPE_STATE.contents)) {
        msg( "ERROR", "TAPE HEAD OUTSIDE BOUNDARY", -5);
    }

    tapechar = TAPE_STATE.contents[pos];
    printf( "ITER %d, PRESENT STATE [%d] AT POSITION = %d; READ CHAR %c\n", NUM_ITERS, TAPE_STATE.pstate, pos, tapechar);

    return tapechar;
}
/* ********************************************************************************************** */


/* ********************************************************************************************** */
int
nextstate(char c) {
    int ps;
    int idx;
    int found;

    ps = TAPE_STATE.pstate;

    found = NOT_FOUND;
    for (idx=0; idx<NUM_STATES; idx++) {
        if ( STATE[idx].pstate == ps )  {
            if (STATE[idx].ifchar == c) {
                found = idx;
                break;
            }
            if (STATE[idx].ifchar == WILDCARD) {
                found = idx;
                break;
            }
        }
    }

    if (found == NOT_FOUND ) {
        msg("ERROR", "REACHED INVALID STATE", -6);
    }

    printf("MATCHING RULE: PS=%d \t IFC=%c \t WRT=%c \t DIR=%d \t NS=%d\n", STATE[idx].pstate, STATE[idx].ifchar, STATE[idx].writechar, STATE[idx].direction, STATE[idx].nstate);
    return (found);
}
/* ********************************************************************************************** */


/* ********************************************************************************************** */
void
update_state(char c, int idx) {
    int ps, direction, nstate;
    char ifchar, writechar;

    ps = STATE[idx].pstate;
    ifchar = STATE[idx].ifchar;
    writechar = STATE[idx].writechar;
    direction = STATE[idx].direction;
    nstate = STATE[idx].nstate;

    TAPE_STATE.position = TAPE_STATE.position + direction;
    TAPE_STATE.direction = direction;
    TAPE_STATE.pstate = nstate;

    if (writechar == WILDCARD) writechar = NOTPRINT;

    TAPE_STATE.traversal[NUM_ITERS]= (char) c;
    TAPE_STATE.output[OUTPUT_IDX]  = (char) writechar;

    printf( "UPDATE: NEXT STATE = %d, POSITION = %d, DIRECTION = %d\n", TAPE_STATE.pstate, TAPE_STATE.position, TAPE_STATE.direction );
    if (writechar != WILDCARD) {
        printf( "GIVEN IFCHAR [%c], WROTE CHAR [%c]\n", ifchar, writechar );
    } else {
        printf( "GIVEN IFCHAR [%c], WROTE CHAR []\n", ifchar );
    }

    if (writechar != NOTPRINT) OUTPUT_IDX++;
    NUM_ITERS++;

    if (NUM_ITERS>=MAXOUT) {
        msg("ERROR", "CAN NOT OPEN SPECIFIED TAPE FILE", -29);
    }

    return;
}
/* ********************************************************************************************** */


/* ********************************************************************************************** */
int
halting_tests() {
    if (TAPE_STATE.pstate == TAPE_STATE.hstate) {
        MSG( "INFO", "HALTING STATE BEEN REACHED." );
        return (0);
    }
    return(1);
}
/* ********************************************************************************************** */


/* ********************************************************************************************** */
void 
print_tape_state() {
    printf( "TAPE CONTENTS : %s",   TAPE_STATE.contents);
    printf( "TAPE TRAVERSAL: %s\n", TAPE_STATE.traversal);
    printf( "OUTPUT STREAM : %s\n", TAPE_STATE.output);
    return;
}
/* ********************************************************************************************** */


/* ********************************************************************************************** */
void main( int argc, char **argv) {
    char *tapefile;
    char c;
    int idx;
    int done = 0;

    if (argc > 1) {
        tapefile = argv[1];
    } else {
        msg("ERROR", "NO SPECIFIED TAPE FILE", -30);
        exit(-30);
    }

    HEADER();
    load_tapefile(tapefile);
    HEADER();

    print_action_table();

    for(;;) {
        done = halting_tests();
        if ( done > 0 ) {
            c = readchar();
            idx = nextstate(c);
            update_state(c, idx);
            print_tape_state();
        } else {
            print_tape_state();
            break;
        }
        HEADER();
    }
    printf( "%s", TAPE_STATE.output);

    exit(0);
}
/* ********************************************************************************************** */


