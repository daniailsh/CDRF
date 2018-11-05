/*************************************************************************/
/*									 */
/*	Main routine, RF						 */
/*	------------------						 */
/*									 */
/*************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "defns.i"
#include "types.i"
#include <string.h>
#include "time.h"

short		MaxDiscrVal = 2;/* max discrete values for any att */
short		MaxAtt;		/* max att number */
short 		MaxClass;	/* max class number */

ItemNo		MaxItem;	/* max data item number */

Description	*Item;		/* data items */
//Description 	*tmp_Item;	/* Random Selection items */


DiscrValue	*MaxAttVal;	/* number of values for each att */

char		*SpecialStatus;	/* special att treatment */

String		*ClassName,	/* class names */
		*AttName,	/* att names */
		**AttValName,	/* att value names */
		FileName = "DF";/* family name of files */

short		VERBOSITY = 0,		/* verbosity level (0 = none) */
		TRIALS    = 10;		/* number of trees to be grown */
int		TESTCNT = 0;
Boolean		GAINRATIO  = true,	/* true=gain ratio, false=gain */
		SUBSET     = false,	/* true if subset tests allowed */
		TRAIN      = true,	
		UNSEENS    = false,	/* true if to evaluate on test data */
		PROBTHRESH = false;	/* true if to use soft thresholds */

ItemNo		MINOBJS   = 2,		/* minimum items each side of a cut */
		WINDOW    = 0,		/* initial window size */
		INCREMENT = 0;		/* max window increment each iteration */

float		CF = 0.25;		/* confidence limit for tree pruning */

Tree		*Pruned;		/////Tree Structure -> types.i

Boolean		AllKnown = true;

int Nomal  = 0;
int Anomaly = 0;
int OK_dev[10];
extern represent;

int TOTAL;
int TEST;
int TREE;

typedef	struct ValRange *RangeDescRec;
struct ValRange
	{
	    Boolean	Known,		/* is range known? */
			Asked;		/* has it been asked? */
	    float	LowerBound,	/* lower bound given */
			UpperBound,	/* upper ditto */
			*Probability;	/* prior prob of each discr value */
	};


extern RangeDescRec RangeDesc;


int maxCnt();
    main(Argc, Argv)
/*  ----  */
    int Argc;
    char *Argv[];
{
    int o;
    int i;
    char *s;
    int NumofTree =200;
    extern char *optarg;
    extern int optind;
    Boolean FirstTime=true;
    short Best, BestTree();
    FILE *temp;
    FILE *fp;

    float test_value[55];
    int numofCTree = 0;
    int initcnt =0;
	
    printf("\n================================================\n");
    Attribute a;

    // Test //
    char *buffer;
    char *last_token;
    char *delimiter = ",\n";
    int cnt_temp=0;
    char buf[128];
    char *class;
    int max = 0;
    char Filename[30];
    char ResultFile[30];
    char ftmp[10];
    int cnt; 
    int compare = 0;
    char *homeinfo;
    char *lat;
    char *lon;

    // Time calculate //
    clock_t before;
    double time_result;
    // Time calculate //

    /*  Process options  */
    int matchcnt[3];
    int cnt_tmp =0;

    while ( (o = getopt(Argc, Argv, "f:bupv:t:w:i:gsm:c:")) != EOF )
    {
	if ( FirstTime )
	{
	    printf("\n    Options:\n");
	    FirstTime = false;
	}

	switch (o)
	{
	case 'f':   FileName = optarg;
		    printf("\tFile stem <%s>\n", FileName);
		    break;
	case 'b':   TRAIN = false;
		    break;
	case 'u':   UNSEENS = true;
		    printf("\tTrees evaluated on unseen cases\n");		//Unseened Data !
		    break;
	case 'p':   PROBTHRESH = true;
		    printf("\tProbability thresholds used\n");
		    break;
	case 'v':   VERBOSITY = atoi(optarg);
		    printf("\tVerbosity level %d\n", VERBOSITY);
		    break;
	case 't':   TESTCNT = atoi(optarg);
		    break;
		    break;
	case 'w':   WINDOW = atoi(optarg);
		    printf("\tInitial window size of %d items\n", WINDOW);
		    Check(WINDOW, 1, 1000000);
		    TRAIN = false;
		    break;
	case 'i':   INCREMENT = atoi(optarg);
		    printf("\tMaximum window increment of %d items\n",
			   INCREMENT);
		    Check(INCREMENT, 1, 1000000);
		    TRAIN = false;
		    break;
	case 'g':   GAINRATIO = false;
		    printf("\tGain criterion used\n");
		    break;
	case 's':   SUBSET = true;
		    printf("\tTests on discrete attribute groups\n");
		    break;
	case 'm':   MINOBJS = atoi(optarg);
		    printf("\tSensible test requires 2 branches with >=%d cases\n",
			    MINOBJS);
		    Check(MINOBJS, 1, 1000000);
		    break;
	case 'c':   NumofTree = atof(optarg);
		    break;
	case '?':   printf("unrecognised option\n");
		    exit(1);
	}
    }
    GetNames();
    if ( TRAIN )
    {
    
    GetData(".data");
    printf("\nRead %d cases (%d attributes) from %s.data\n",
	   MaxItem+1, MaxAtt+1, FileName);
    TOTAL =  MaxItem+1;
    TEST = (MaxItem+1)*0.7;
    TREE = NumofTree;
    //printf("TOTAL %d and TEST %d and Tree %d\n",TOTAL,TEST,TREE);
    }
    printf("================================================\n");


    if ( TRAIN )
    {
	printf("Create Forest\n");
	TRIALS = 1;
	before = clock();
	numofCTree = OneTree(NumofTree);
	time_result = (double)(clock() - before) / CLOCKS_PER_SEC;
	printf("Time to create Forest is %f \n",time_result);
	Best = 0;
	for(i =0;i<numofCTree ;i++)
    	{
		SaveTree(Pruned[i],".unpruned",i+1);
    	}
    }
    else
    {
	printf("Load Forest(%d Trees)\n",NumofTree);
	Pruned = (Tree *) calloc(NumofTree, sizeof(Tree));
	for(i=0;i<NumofTree ;i++)
	{
		printf("Load %d Tree\n",i+1);
		Pruned[i]=GetTree(".unpruned",i+1);	
	}
    }

    if ( PROBTHRESH )
    {
	printf("Softening thresholds");
	if ( ! TRAIN ) printf(" for best tree from trial %d", Best);
	printf("\n");
	SoftenThresh(Pruned[Best]);
	printf("\n");
	PrintTree(Pruned[Best]);
    }


////////////////////////////////////////////////////////////////////////////Voting

        RangeDesc = (struct ValRange *) calloc(MaxAtt+1, sizeof(struct ValRange));

	ForEach(a, 0, MaxAtt)
    	{
		if ( MaxAttVal[a] )
		{
	    	RangeDesc[a].Probability =
			(float *) calloc(MaxAttVal[a]+1, sizeof(float));
		}
    	}
	
	Clear();

	// Test Data Start //
	printf("================================================\n");
	if( TRAIN == false)
	{
	printf("TEST Start \n");
	
        for(cnt = 0 ; cnt < 1; cnt++)
	{

	memset(Filename,0,30);
	strcpy(Filename,"testdata");
	sprintf(ftmp, "%d", cnt+1);
	strcat(Filename,ftmp);
	strcat(Filename,".txt");
	
	temp = fopen(Filename,"r");

	memset(ResultFile,0,30);
	strcpy(ResultFile,"result");
	sprintf(ftmp, "%d", cnt+1);
	strcat(ResultFile,ftmp);
	strcat(ResultFile,".txt");

	fp = fopen(ResultFile,"w");
	
	buffer = (char *) calloc(4096, sizeof(char));	//Seokhwan use
   	for(cnt = 0 ; cnt < 3 ; cnt++)
		matchcnt[cnt] = 0;
	while(fgets(buffer, 4096, temp) != NULL)
	{
		cnt_temp++;
		last_token = strtok(buffer,delimiter);
		for(i =0; i< 784; i++)
		{
			test_value[i] = atof(last_token);
			last_token = strtok(NULL, delimiter);
		}
		class = last_token;
		//printf("Class = %s\n",class);
	
		for(i = 0 ; i<MaxClass+1; i++)
		{
			OK_dev[i] = 0;
		}
		for(i =0;i<NumofTree ;i++)
    		{//NumofTree is not equal SDRF case... 
			InterpretTree(Pruned[i],test_value, 1); //1 == flag(printf ok)
    		}
		max = maxCnt();
		compare = atoi(class);
		fprintf(fp,"%d,%d",compare,max);
		
		if(compare==max)
		{
			fprintf(fp,",Match,0\n");
			matchcnt[0]++;
			
		}
		else if(compare > max)
		{
			fprintf(fp,",High,%d\n",compare-max);
			matchcnt[1]++;
		}
		else
		{
			fprintf(fp,",Low,%d\n",max-compare);
			matchcnt[2]++;
		}
		for(initcnt =0 ; initcnt <10 ; initcnt++)
		OK_dev[initcnt]  = 0;
		represent = 0;
		cnt_tmp++;
		if(cnt_tmp ==TESTCNT)
			break;
	
	}
	fprintf(fp,"================================================\n");
	fprintf(fp,"Matched is %d, %.2f%%\n",matchcnt[0],((double)matchcnt[0]/(matchcnt[0]+matchcnt[1]+matchcnt[2]))*100);
	printf("\nTEST End\n");
	printf("================================================\n");
	
	
	free(buffer);
	fclose(temp);
	fclose(fp);
	}
	} 
	// Test Data End //
	
/////////////////////////////////////////////////////////////////////////////////////////

    exit(0);
}
int maxCnt()
{
	int i = 0;
	int max = 0 ;	
	for(i = 1 ; i< 10; i++)
	{
		if(OK_dev[max] < OK_dev[i])
		{
			max = i;
		}

	}
	return max;
}
