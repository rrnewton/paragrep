/* HELP - help database accessing program
** Author: Ronald L. Rivest
** See HELP.DOC for documentation
** 10/14/88
** .f and .n flags added by Michael Ernst, 7/14/90.
** .w flag added, MissFile output cleaned up by Michael Ernst, 11/29/90.
** \f made a whitespace character by Michael Ernst, 3/6/91.
** .F flag added by Michael Ernst, 7/19/92.
** Modified for DOS by Michael Ernst, 10/30/92 - 11/5/92.
** Indication of overflow of filenames array added by Michael Ernst, 7/19/93.
** Permit flags preceded by -; add .- and -. flags.  Michael Ernst, 12/1/94.
** Expand "~/", add return types, add exit status.  Michael Ernst, 10/18/96.
** Print warning if file not found.  Michael Ernst, 10/3/2000.
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define NOGLOBALHELP

#ifdef MSDOS
 #include <time.h>
 #ifndef MISSFILENAME
  #define MISSFILENAME "c:\\HELPNEED.TXT"
 #endif
 #ifndef GLOBALHELPFILENAME
  #define GLOBALHELPFILENAME "c:\\HELP.TXT"
 #endif
 #ifndef HOMEDIRDIR
  #define HOMEDIRDIR "c:\\users\\"
 #endif
 #define FILESEPCHAR '|'
 #define FILESEPSTRING "|"
 #define NOWIZ
 #define NOGLOBALHELP
#else
 #ifndef MISSFILENAME
  #define MISSFILENAME "/a/aviary/HELP.NEEDED"
 #endif
 #ifndef NOGLOBALHELP
  #ifndef GLOBALHELPFILENAME
   #define GLOBALHELPFILENAME "/a/aviary/HELP"
  #endif
 #endif
 #ifndef HOMEDIRDIR
  #define HOMEDIRDIR "/homes/gws/"
 #endif
 #define FILESEPCHAR ':'
 #define FILESEPSTRING ":"
#endif

/********************/
/* GLOBAL VARIABLES */
/********************/

char *SP[50];        /* Search patterns */
int  NSP;            /* Number of search patterns */

// RRN: Uhh, what a loser language.  I'm changing this...
//#define FILENAMESSIZE 4000
#define FILENAMESSIZE 20000
char filenames[FILENAMESSIZE]; /* list of files to search, separated by FILESEPCHAR's */
char *filenameptr;    /* current file name pointer (points within filenames) */
char filename[200];   /* current file name */
FILE *DataFile;       /* file handle number for current file */
FILE *MissFile;       /* Record missed help queries */
/* Each paragraph may have up to 200 lines of 200 characters each */
char L[200][200];    /* buffer for lines */
int  NL;             /* number of lines actually read */
int  LinesPerPage = 20;   /* Number of lines to output before "more" */
int  LineCounter = 0;     /* Lines output this page */

int  Hit;            /* True if some output produced */

#define TRUE  1
#define FALSE 0
typedef int Bool;

int EOFSeen;

/* OPTION SWITCHES */
int OnlyDotOptions, MoreProcessing, ShowFilename, NoWizards, NoGlobalHelpFile;

/**************/
/* PROCEDURES */
/**************/

char upper[256]; /* upper case version */

void Initupper()
{ int i;
  for (i=0;i<256;i++) upper[i] = i;
  for (i='a'; i<='z'; i++) upper[i] = i - 'a' + 'A';
}

Bool OpenNextDatabase()
{ char *c;
  while (*filenameptr)
    { if (*filenameptr==FILESEPCHAR)
        filenameptr++;
      /* Don't complain about empty file. */
      if (*filenameptr == FILESEPCHAR)
        continue;
      /* Copy next file name into filename */
      c = filename;
      if (*filenameptr == '~')
	{ if (filenameptr[1] == '/')
            { char *p = getenv("HOME");
              if (p != NULL)
                { *c = 0;
                  strcpy(c,p);
                  c = c+strlen(c);
                  filenameptr++;
                }
            }
          else
            { *c = 0;
              strcpy(c,HOMEDIRDIR);
              c = c+strlen(c);
              filenameptr++;
            }
        }
      while (*filenameptr && *filenameptr != FILESEPCHAR) *c++ = *filenameptr++;
      *c = 0;
      DataFile = fopen(filename, "r");
      if (DataFile == NULL)
	{ printf("Warning: can't open file: %s\n",filename);
	  continue;
	}
      /* printf("Searching %s\n",filename); */
      EOFSeen = FALSE;
      return(TRUE);
    }
  return(FALSE);
}

Bool IsWhiteSpace(c)
char c;
{ if (c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == '\f')
    return(TRUE);
  else
    return(FALSE);
}

Bool IsPrefix(x,y)
char *x, *y;
/* returns TRUE if x is a prefix of y , else FALSE */
{ while (upper[(int)*x] == upper[(int)*y] && *x && *y)  { x++; y++; }
  if (*x == 0) return(TRUE);
  return(FALSE);
}

Bool IsSubstring(x,y)
char *x, *y;
/* returns TRUE if x is a substring of y, else FALSE */
{ int c;
  if (*x == 0) return(TRUE);
  c = upper[(int)*x];
  while (*y) { if (c==upper[(int)*y] && IsPrefix(x,y)) return(TRUE);
	       y++;
	     }
  return(FALSE);
}

int ReadLine()
/* Read a line into L[NL] */
/* Returns  0 if a totally blank line read
**          1 if a nonblank line read (and NL is incremented)
**          2 if a "\include{filename}" line was read
**          3 if end-of-file condition
*/
{ char *c,*d;
  if (EOFSeen) return(2);
  if (NULL==fgets(L[NL],200,DataFile))
     /* End of file */
    { EOFSeen = TRUE; return(3); }
  else if (L[NL][0] == '%')
    return(1);
  else if (IsPrefix("\\include{",L[NL]))
    { /* append filename to end of filesnames string */
      c = d = L[NL]+strlen("\\include{");
      while (*c && *c != '}') c++;
      *c = 0;
      if (strlen(d)+strlen(filenames)<(FILENAMESSIZE-2))
	  { strcat(filenames,FILESEPSTRING);
	    strcat(filenames,d);
	  }
      else
	  { printf("Too many included files -- increase FILENAMESSIZE.\n");
	    exit(2);
	  }
      return(1);
    }
  else
    { c = L[NL];
      while (*c)
	{ if (!IsWhiteSpace(*c))
	    { /* nonblank line read */
	      if (NL < 199) NL++;
	      return(1);
	    }
	  c++;
	}
      /* blank line read */
      return(0);
    }
}

Bool ReadPara()
/* returns TRUE if para actually read, otherwise FALSE */
{ NL = 0;
  if (EOFSeen) return(FALSE);
  /* Skip any initial blank lines */
  while (ReadLine() == 0) ;
  /* Check for EOF */
  if (EOFSeen) return(FALSE);
  /* Collect remaining nonblank lines of paragraph */
  while (ReadLine() == 1) ;
  return(TRUE);
}

void CloseDatabase()
/* closes the input file */
{ fclose(DataFile);
}

void DoMoreProcessing()
/* Ask for typein to see more output */
{ if (MoreProcessing && LineCounter == LinesPerPage)
    { printf("-- Press carriage return for more --");
      getchar();
      LineCounter = 0;
    }
}

void DoShowFilename()
/* Show name of file before paragraph */
{ if (ShowFilename)
    { printf("[%s]\n",filename);
      LineCounter++;
    }
}

void PrintPara()
/* prints all lines.
*/
{ int i;
  char *x;
  DoShowFilename();
  for (i=0;i<NL;i++)
      { x = L[i];
	DoMoreProcessing();
	printf("%s",x);
	LineCounter++;
      }
  DoMoreProcessing();
  printf("\n");
  LineCounter++;
}

Bool MatchLinePattern(line,x)
char *line, *x;
{ return(IsSubstring(x,line));
}

Bool MatchParaPattern(x)
char *x;
/* returns TRUE if this para contains pattern x */
{ int i;
  for (i=0;i<NL;i++)
    if (MatchLinePattern(L[i],x)) return(TRUE);
  return(FALSE);
}

Bool MatchPara()
/* returns TRUE if this para matches search spec, else FALSE */
{ int i;
  /* now check that para meets all the specs */
  for (i=0;i<NSP;i++)
    /* check that para matches this spec */
    if (!MatchParaPattern(SP[i])) return(FALSE);
  return(TRUE);
}

void ParseCommand(argc,argv)
int argc;
char *argv[];
{ int i;
  NSP = 0;
  MoreProcessing = FALSE;
  ShowFilename = FALSE;
#ifdef NOWIZ
  NoWizards = TRUE;
#else
  NoWizards = FALSE;
#endif
#ifdef NOGLOBALHELP
  NoGlobalHelpFile = TRUE;
#else
  NoGlobalHelpFile = FALSE;
#endif
#ifdef ONLYDOTOPTIONS
  OnlyDotOptions = TRUE;
#else
  OnlyDotOptions = FALSE;
#endif
  /* Now process options one at a time */
  for (i=1;i<argc;i++)
    { /* check for option choice for "more" processing */
      if ((argv[i][0] == '.' || (argv[i][0] == '-' && !OnlyDotOptions))
	  && argv[i][2] == 0)
	{ if (argv[i][1] == 'M' || argv[i][1] == 'm')
	    MoreProcessing = TRUE;
	/* check for option choice for showing filenames */
	else if (argv[i][1] == 'N' || argv[i][1] == 'n')
	  ShowFilename = TRUE;
	/* check for explicit filename instead of $HOME/HELP */
	else if ((argv[i][1] == 'F' || argv[i][1] == 'f')
		 && i != argc-1)
	  { if (argv[i][1] == 'F')
	      { NoGlobalHelpFile = TRUE;
		NoWizards = TRUE;
	      }
	    strcat(filenames,argv[++i]);
	    strcat(filenames,FILESEPSTRING);
	  }
	/* check for option choice:  don't ask wizards for help */
	else if (argv[i][1] == 'W' || argv[i][1] == 'w')
	  NoWizards = TRUE;
	else if (argv[i][0] == '.' || argv[i][1] == '-')
	  OnlyDotOptions = FALSE;
	else if (argv[i][0] == '-' || argv[i][1] == '.')
	  OnlyDotOptions = TRUE;
	/* it is input specification (or bad flag treated as input spec) */
	else SP[NSP++] = argv[i];
       }
      /* it is input specification */
      else SP[NSP++] = argv[i];
    }
  /* If no arguments were given, assume "general help" was given */
  if (NSP == 0)
    { SP[NSP++] = "general";
      SP[NSP++] = "help";
    }
}

void SearchFiles()
{ while (OpenNextDatabase())
    { while (ReadPara())
	if (MatchPara())
	  { Hit = TRUE;
	    PrintPara();
	  }
      CloseDatabase();
    }
}

int main(argc,argv)
int argc;
char *argv[];
{ int i;
  char *p;
  char *getenv();
  char datestring[100];
  Initupper();
  *filenames = 0;
  ParseCommand(argc,argv);
  if (*filenames == 0)
    /* no filename specified on command line; ie, filenames is still null */
    { p = getenv("HOME");
      if (p != NULL)
	{   strcat(filenames,p);
	    strcat(filenames,"/HELP");
	    strcat(filenames,FILESEPSTRING);
	  }
      }
#ifndef NOGLOBALHELP
  if (!(NoGlobalHelpFile))
    strcat(filenames,GLOBALHELPFILENAME);
#endif
  filenameptr = filenames;
  Hit = FALSE;
  SearchFiles();
  if (!Hit)
    { printf("Sorry...no information available matching your request...\n");
      if (!NoWizards)
	{ printf("Your request has been logged on %s...\n",MISSFILENAME);
	  printf("(You may hear from a wizard.)\n");
	  /* Save record of missed query */
	  MissFile = fopen(MISSFILENAME,"a");
	  if (MissFile != NULL)
	    { fputs("--------------------------------\n",MissFile);
	      fputs(getenv("USER"),MissFile);
	      fputs(":",MissFile);
	      fputs("help ",MissFile);
	      for (i=0;i<NSP;i++)
		{ fputs(SP[i],MissFile);
		  fputs(" ",MissFile);
		}
	      fputs("\n",MissFile);
#ifdef DOS
	      _strdate(datestring);
	      fputs(datestring,MissFile);
	      fputs("\n",MissFile);
	      fclose(MissFile);
#else
	      fclose(MissFile);
	      *datestring = 0;
	      strcat(datestring, "date >> ");
	      strcat(datestring, MISSFILENAME);
	      system(datestring);
#endif
	    }
	}
    }
  /* Like grep, exit status is 0 if any matches are found, 1 if none. */
  if (Hit)
    return 0;
  else
    return 1;
}
