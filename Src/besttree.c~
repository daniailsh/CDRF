/*************************************************************************/
/*									 */
/*	Routines to manage tree growth, pruning and evaluation		 */
/*	------------------------------------------------------		 */
/*									 */
/*************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <string.h>
#include <unistd.h>
#include "defns.i"
#include "types.i"
#include "extern.i"


/* Using Weighted Random Selections */
//#define ALPHA 890
//#define TEST 623
//#define TREESIZE 10

int		TEST;
int		TOTAL;
/* Using Tree_based OOB */
//Description	tmp_Item[TREESIZE][TEST];
//Description	new_tmp_Item[TREESIZE][TEST];
Description	tmp_Item[500][200000];
Description	new_tmp_Item[500][200000];


Description	MCnemarItem[100000];
int BEFORE[500000];
int AFTER[500000];


int isFirst = 1;
/* Using Tree_based OOB */

int top = 0;

ItemNo		MaxItem;	/* max data item number */
ItemNo		*TargetClassFreq;
Tree		*Raw;
extern Tree	*Pruned;
extern int	oobval;

double		*WeightofItem;
double		*Next_WeightofItem;

typedef	struct ValRange *RangeDescRec;
struct ValRange
	{
	    Boolean	Known,		/* is range known? */
			Asked;		/* has it been asked? */
	    float	LowerBound,	/* lower bound given */
			UpperBound,	/* upper ditto */
			*Probability;	/* prior prob of each discr value */
	};
RangeDescRec RangeDesc;

double cal_weight_Tree(int NumofTree,double *WeightofItem);
double cal_OOB(Tree Raw, int n);

int isCDRF_First = 1;
int Test_cnt = 0;
double covariance[500];
//double covariance[TREESIZE];
double Cov[2][2];
double Cov2[500][4];
//double Cov2[TREESIZE][2];
int CDRFv2(Tree* Raw, int cnt);
FILE *fp;

int TT[10];
int FT[10];
int TF[10];
int iSF = 0;


int MCnemar(Tree* Raw, int cnt);
int isMC_First = 1;

void InitItemWeight(void)
{
    ItemNo i;

    ForEach(i, 0, MaxItem+1)
    {
        WeightofItem[i] = 1.0/(MaxItem+1);
    }
}

double randomDouble(void)
{
	return (double)rand() / RAND_MAX;
}


void swap_weight(double* a, double* b) {
	double tmp = *a;
	*a = *b;
	*b = tmp;
}
void swap_int(Description* a, Description* b) {
	Description tmp = *a;
	*a = *b;
	*b = tmp;
}

/*************************************************************************/
/*									 */
/*	Grow and prune a single tree from all data			 */
/*									 */
/*************************************************************************/


    OneTree(NumofTree)
/*  ---------  */
{
    int cnt=0;
    int i=0;
    int j=0;
    TOTAL = (int)MaxItem*0.7;

    /* calculate OOB error */
    double oob_result;
    /* calculate OOB error */
    /* Using Weighted Random Selections */
    Description swap_tmp;
    WeightofItem = (double *) calloc(MaxItem+1, sizeof(double));
    Next_WeightofItem = (double *) calloc(MaxItem+1, sizeof(double));
    double key[200000];
    //double key[ALPHA];
    double tmp_key;
    double tmp_result;
    InitItemWeight();
    int maxkeyindex = 0;
    /* Using Weighted Random Selections */

    /* Using Calculate Weight */
    double Z;
    /* Using Calculate Weight */
    /* Using C-DRF */
    int cdrf_result = 0;
    int numOfCTree = 0;
    fp = fopen("CDRF Result","w");

    FILE *ff;
    ItemNo This, Alt, Left;
    /* Using C-DRF */

    char sh[30];
    char shbuf[10];
    int changecnt =0;
    Tree FormTree(), CopyTree();
    Boolean Prune();

    InitialiseTreeData(); // Point 1
    InitialiseWeights();  // Dynamic RF use

    srand(time(NULL));

    Raw = (Tree *) calloc(NumofTree, sizeof(Tree));	
    Pruned = (Tree *) calloc(NumofTree, sizeof(Tree)); 
    
    AllKnown = true;

    for ( cnt = 0; cnt < NumofTree ; cnt++)
    { //Create Random Tree
	if(cnt == 0)	
	Shuffle();
    if(cnt >0)
    {
	This = 0;
    for( Left = MaxItem ; Left ; )
    {
        //Alt = This + (Left--) * Random;
	Alt = This + (Left--) * ((rand()&2147483647) / 2147483648.0);
        //printf("%f  ",Alt);
	swap_int(&Item[This], &Item[Alt]);
	swap_weight(&key[This], &key[Alt]);
	swap_weight(&WeightofItem[This], &WeightofItem[Alt]);
	swap_weight(&Next_WeightofItem[This], &Next_WeightofItem[Alt]);
   	This ++;
    }
}
	/* Using Weighted Random Selections */
	Raw[cnt] = FormTree(0, TEST);
	/* Using Weighted Random Selections */
	
	//This is CDRF Function //
	cdrf_result = CDRFv2(Raw,cnt);
	//if(cnt == 79 || cnt == 99)
	//cdrf_result = MCnemar(Raw,cnt);
	//This is CDRF Function //	

	//oob_result = cal_OOB(Raw[cnt], cnt);
	/*if(oob_result >= 0.2)
	{
		cal_weight_Tree(cnt+1,WeightofItem); //OoB Tree Based
		UpdateWeight(cnt+1);  //OoB Data Based
	}*/

	if(cdrf_result == 1)
	{
		numOfCTree = cnt;	
		break;
	}
	numOfCTree = cnt;
    }

    for (cnt = 0; cnt < numOfCTree+1 ; cnt++)
    {
    	Pruned[cnt] = CopyTree(Raw[cnt]);
    }
	fclose(fp);
	free(Raw);
	
	sprintf(sh,"cat /proc/%d/status",getpid());	
	system((char*)sh);

	return numOfCTree+1;
}



/*************************************************************************/
/*									 */
/*	Grow and prune TRIALS trees and select the best of them		 */
/*									 */
/*************************************************************************/


short BestTree()
/*    --------  */
{
    Tree CopyTree(), Iterate();
    Boolean Prune();
    short t, Best=0;

    InitialiseTreeData();

    TargetClassFreq = (ItemNo *) calloc(MaxClass+1, sizeof(ItemNo));

    Raw    = (Tree *) calloc(TRIALS, sizeof(Tree));
    Pruned = (Tree *) calloc(TRIALS, sizeof(Tree));

    /*  If necessary, set initial size of window to 20% (or twice
	the sqrt, if this is larger) of the number of data items,
	and the maximum number of items that can be added to the
	window at each iteration to 20% of the initial window size  */

    if ( ! WINDOW )
    {
	WINDOW = Max(2 * sqrt(MaxItem+1.0), (MaxItem+1) / 5);
    }

    if ( ! INCREMENT )
    {
	INCREMENT = Max(WINDOW / 5, 1);
    }

    FormTarget(WINDOW);

    /*  Form set of trees by iteration and prune  */

    ForEach(t, 0, TRIALS-1 )
    {
        FormInitialWindow();

	printf("\n--------\nTrial %d\n--------\n\n", t);

	Raw[t] = Iterate(WINDOW, INCREMENT);
	printf("\n");
	PrintTree(Raw[t]);

	SaveTree(Raw[t], ".unpruned");

	Pruned[t] = CopyTree(Raw[t]);
	if ( Prune(Pruned[t]) )
	{
	    printf("\nSimplified ");
	    PrintTree(Pruned[t]);
	}

	if ( Pruned[t]->Errors < Pruned[Best]->Errors )
	{
	    Best = t;
	}
    }
    printf("\n--------\n");

    return Best;
}



/*************************************************************************/
/*									 */
/*  The windowing approach seems to work best when the class		 */
/*  distribution of the initial window is as close to uniform as	 */
/*  possible.  FormTarget generates this initial target distribution,	 */
/*  setting up a TargetClassFreq value for each class.			 */
/*									 */
/*************************************************************************/


    FormTarget(Size)
/*  -----------  */
    ItemNo Size;
{
    ItemNo i, *ClassFreq;
    ClassNo c, Smallest, ClassesLeft=0;

    ClassFreq = (ItemNo *) calloc(MaxClass+1, sizeof(ItemNo));

    /*  Generate the class frequency distribution  */

    ForEach(i, 0, MaxItem)
    {
	ClassFreq[ Class(Item[i]) ]++;
    }

    /*  Calculate the no. of classes of which there are items  */

    ForEach(c, 0, MaxClass)
    {
	if ( ClassFreq[c] )
	{
	    ClassesLeft++;
	}
	else
	{
	    TargetClassFreq[c] = 0;
	}
    }

    while ( ClassesLeft )
    {
	/*  Find least common class of which there are some items  */

	Smallest = -1;
	ForEach(c, 0, MaxClass)
	{
	    if ( ClassFreq[c] &&
		 ( Smallest < 0 || ClassFreq[c] < ClassFreq[Smallest] ) )
	    {
		Smallest = c;
	    }
	}

	/*  Allocate the no. of items of this class to use in the window  */

	TargetClassFreq[Smallest] = Min(ClassFreq[Smallest], Round(Size/ClassesLeft));

	ClassFreq[Smallest] = 0;

	Size -= TargetClassFreq[Smallest];
	ClassesLeft--;
    }

    cfree(ClassFreq);
}



/*************************************************************************/
/*									 */
/*  Form initial window, attempting to obtain the target class profile	 */
/*  in TargetClassFreq.  This is done by placing the targeted number     */
/*  of items of each class at the beginning of the set of data items.	 */
/*									 */
/*************************************************************************/


    FormInitialWindow()
/*  -------------------  */
{
    ItemNo i, Start=0, More;
    ClassNo c;
    void Swap();

    Shuffle();

    ForEach(c, 0, MaxClass)
    {
	More = TargetClassFreq[c];

	for ( i = Start ; More ; i++ )
	{
	    if ( Class(Item[i]) == c )
	    {
		Swap(Start, i);
		Start++;
		More--;
	    }
	}
    }
}



/*************************************************************************/
/*									 */
/*		Shuffle the data items randomly				 */
/*									 */
/*************************************************************************/


    Shuffle()
/*  -------  */
{
    ItemNo This, Alt, Left;
    Description Hold;
   
    This = 0;
    for( Left = MaxItem+1 ; Left ; )
    {
        //Alt = This + (Left--) * Random;
	Alt = This + (Left--) * ((rand()&2147483647) / 2147483648.0);
        Hold = Item[This];
        Item[This++] = Item[Alt];
        Item[Alt] = Hold;
    }
}

	

/*************************************************************************/
/*									 */
/*  Grow a tree iteratively with initial window size Window and		 */
/*  initial window increment IncExceptions.				 */
/*									 */
/*  Construct a classifier tree using the data items in the		 */
/*  window, then test for the successful classification of other	 */
/*  data items by this tree.  If there are misclassified items,		 */
/*  put them immediately after the items in the window, increase	 */
/*  the size of the window and build another classifier tree, and	 */
/*  so on until we have a tree which successfully classifies all	 */
/*  of the test items or no improvement is apparent.			 */
/*									 */
/*  On completion, return the tree which produced the least errors.	 */
/*									 */
/*************************************************************************/


Tree Iterate(Window, IncExceptions)
/*   -------  */
    ItemNo Window, IncExceptions;
{
    Tree Classifier, BestClassifier=Nil, FormTree();
    ItemNo i, Errors, TotalErrors, BestTotalErrors=MaxItem+1,
	   Exceptions, Additions;
    ClassNo Assigned, Category();
    short Cycle=0;
    void Swap();

    printf("Cycle   Tree    -----Cases----");
    printf("    -----------------Errors-----------------\n");
    printf("        size    window   other");
    printf("    window  rate   other  rate   total  rate\n");
    printf("-----   ----    ------  ------");
    printf("    ------  ----  ------  ----  ------  ----\n");

    do
    {
	/*  Build a classifier tree with the first Window items  */

	InitialiseWeights();
	AllKnown = true;
	printf("HereHereHereHere You!\n");
	Classifier = FormTree(0, Window-1);
	
	/*  Error analysis  */

	Errors = Round(Classifier->Errors);

	/*  Move all items that are incorrectly classified by the
	    classifier tree to immediately after the items in the
	    current window.  */

	Exceptions = Window;
	ForEach(i, Window, MaxItem)
	{
	    Assigned = Category(Item[i], Classifier);
	    if ( Assigned != Class(Item[i]) )
	    {
		Swap(Exceptions, i);
		Exceptions++;
	    }
	}
        Exceptions -= Window;
	TotalErrors = Errors + Exceptions;

	/*  Print error analysis  */

	printf("%3d  %7d  %8d  %6d  %8d%5.1f%%  %6d%5.1f%%  %6d%5.1f%%\n",
	       ++Cycle, TreeSize(Classifier), Window, MaxItem-Window+1,
	       Errors, 100*(float)Errors/Window,
	       Exceptions, 100*Exceptions/(MaxItem-Window+1.001),
	       TotalErrors, 100*TotalErrors/(MaxItem+1.0));

	/*  Keep track of the most successful classifier tree so far  */

	if ( ! BestClassifier || TotalErrors < BestTotalErrors )
	{
	    if ( BestClassifier ) ReleaseTree(BestClassifier);
	    BestClassifier = Classifier;
	    BestTotalErrors = TotalErrors;
        }
	else
	{
	    ReleaseTree(Classifier);
	}

	/*  Increment window size  */

	Additions = Min(Exceptions, IncExceptions);
	Window = Min(Window + Max(Additions, Exceptions / 2), MaxItem + 1);
    }
    while ( Exceptions );

    return BestClassifier;
}



/*************************************************************************/
/*									 */
/*	Print report of errors for each of the trials			 */
/*									 */
/*************************************************************************/


    Evaluate(CMInfo, Saved)
/*  --------  */
    Boolean CMInfo;
    short Saved;
{
    ClassNo RealClass, PrunedClass, Category();
    short t;
    ItemNo *ConfusionMat, i, RawErrors, PrunedErrors;

    if ( CMInfo )
    {
	ConfusionMat = (ItemNo *) calloc((MaxClass+1)*(MaxClass+1), sizeof(ItemNo));
    }

    printf("\n");

    if ( TRIALS > 1 )
    {
	printf("Trial\t Before Pruning           After Pruning\n");
	printf("-----\t----------------   ---------------------------\n");
    }
    else
    {
	printf("\t Before Pruning           After Pruning\n");
	printf("\t----------------   ---------------------------\n");
    }
    printf("\tSize      Errors   Size      Errors   Estimate\n\n");

    ForEach(t, 0, TRIALS-1)
    {
	RawErrors = PrunedErrors = 0;

	ForEach(i, 0, MaxItem)
	{
	    RealClass = Class(Item[i]);

	    if ( Category(Item[i], Raw[t]) != RealClass ) RawErrors++;

	    PrunedClass = Category(Item[i], Pruned[t]);

	    if ( PrunedClass != RealClass ) PrunedErrors++;

	    if ( CMInfo && t == Saved )
	    {
		ConfusionMat[RealClass*(MaxClass+1)+PrunedClass]++;
	    }
	}
    
	if ( TRIALS > 1 )
	{
	    printf("%4d", t);
	}

	printf("\t%4d  %3d(%4.1f%%)   %4d  %3d(%4.1f%%)    (%4.1f%%)%s\n",
	       TreeSize(Raw[t]), RawErrors, 100.0*RawErrors / (MaxItem+1.0),
	       TreeSize(Pruned[t]), PrunedErrors, 100.0*PrunedErrors / (MaxItem+1.0),
	       100 * Pruned[t]->Errors / Pruned[t]->Items,
	       ( t == Saved ? "   <<" : "" ));
    }

    if ( CMInfo )
    {
	PrintConfusionMatrix(ConfusionMat);
	free(ConfusionMat);
    }
}
/* Calculate OOB Error */

double cal_OOB(Tree Raw, int n)
{
   int i = 0; 
   int j = 0;
   int a = 0;
   int misscnt = 0;
   double result; 
   float* ItemAttVal;
   
   ItemAttVal = (float *) calloc(MaxAtt, sizeof(float));
   RangeDesc = (struct ValRange *) calloc(MaxAtt+1, sizeof(struct ValRange));
   for(a=0 ; a<MaxAtt+1; ++a)
   {
	if ( MaxAttVal[a] )
	{
	RangeDesc[a].Probability =
		(float *) calloc(MaxAttVal[a]+1, sizeof(float));
	}
   }
   Clear();
  for(j = TEST ; j < MaxItem ; j++)
   {
   	for(i = 0; i<MaxAtt+1; i++)
   	{
		ItemAttVal[i] = CVal(Item[j],i); 
   	}	

    	InterpretTree(Raw,ItemAttVal,0);
	if(oobval != Class(Item[j]))
		misscnt++;
    }
	
   	result = ((double)misscnt / (MaxItem - TEST+1))*100 ;
	printf("misscnt is %d ", misscnt);
	printf("(MaxItem - TEST) is %d, and OObErr is %.2f%\n", MaxItem - TEST+1, result); 
	misscnt = 0;
	//free(ItemAttVal);
	//free(RangeDesc);
	return result; 

}
/* Calculate OOB Error */
/* Using Calculate Weight(OOB_Tree based) */
double cal_weight_Tree(int NumofTree, double *WeightofItem)
{
   int i = 0 ; 
   int j = 0 ;
   int k = 0 ;
   int isBreak = 0;
   float* ItemAttVal;
   int Hoob = 0;
   int cnt = 0;
   int breakcnt = 0;
   double W;
   double Z; 
   
   
   ItemAttVal = (float *) calloc(MaxAtt, sizeof(float));
   RangeDesc = (struct ValRange *) calloc(MaxAtt+1, sizeof(struct ValRange));

	for(i=0 ; i<MaxAtt+1; ++i)
    	{
		if ( MaxAttVal[i] )
		{
	    	RangeDesc[i].Probability =
			(float *) calloc(MaxAttVal[i]+1, sizeof(float));
		}
    	}
	Clear();


   if(isFirst == 1)
   {
	for(i =0 ; i < TEST ; i++)
	{
		tmp_Item[0][i] = Item[i]; 
	}	
	isFirst = 0;
   }   
   else
   {//here I am
	
	for(i = 0 ; i < NumofTree-1 ; i++){
		for(j = 0 ; j < TEST ; j++)
		{
			new_tmp_Item[i][j] = tmp_Item[i][j];
		}
	}
	
	// Reallocate
	for(i = 0 ; i < NumofTree-1 ; i++)
		for(j = 0 ; j < TEST ; j++)
			tmp_Item[i][j] = new_tmp_Item[i][j];
   
	// Allocate
	for(i =0 ; i < TEST ; i++)
		tmp_Item[NumofTree-1][i] = Item[i];

   }

   for(i = 0 ; i < MaxItem+1 ; i++)
   {
	for(j = 0; j<MaxAtt+1; j++)
 	{
		ItemAttVal[j] = CVal(Item[i],j);
   	}
	
	for(j = 0; j < NumofTree; j++)
	{
		isBreak = 0;
		for(k = 0 ; k < TEST; k++)
		{
			if(Item[i] == tmp_Item[j][k])
			{
				isBreak = 1;
			}
			if(isBreak == 1)
			{
				breakcnt++;
				break;
			}
		}
		
		if(isBreak == 0)
		{
			Hoob++;
			InterpretTree(Raw[j],ItemAttVal,0);
			if(oobval == Class(Item[i]))
				cnt++;
		}
	}
	if(breakcnt == NumofTree)
	{
		Next_WeightofItem[i] = WeightofItem[i];
		Z = Z + Next_WeightofItem[i];
	}
	else
	{	
		/*if(cnt == 0)
		W = 0.1;
		else
		W = ((1.0/Hoob)*cnt);
		*/
		W = 1 - ((1.0/Hoob)*cnt);
		Next_WeightofItem[i] = W;
		Z = Z + Next_WeightofItem[i];
	}
	breakcnt = 0;
	cnt = 0;
	Hoob = 0;
	
   }
   for(i =0 ; i< MaxItem+1 ; i++)
   {
	Next_WeightofItem[i] = Next_WeightofItem[i] / Z;
	WeightofItem[i] = Next_WeightofItem[i];
	
   }
	Z = 0; 
}

/* Using Calculate Weight(OOB_Tree based) */

/* Using Calculate Weight */
int CDRFv2(Tree* Raw, int cnt)
{
   int i,n,a,count,j;
   int misscnt = 0;
   int truecnt = 0;
   int totalcnt = 0;
   double recall = 0;
   double Fmeasure = 0;
   int mid = 0;
 
   int CDRFcnt =0;
   
   double Ex = 0;
   double Ey = 0;   
   double Ez = 0;
   double Ev = 0;
   double recall_x = 0;
   double recall_y = 0;
   double recall_z = 0;
   double recall_v = 0;
   double Fmeasure_x = 0;
   double Fmeasure_y = 0;
   double Fmeasure_z = 0;
   double result = 0;
   double result2 =0;
   double result3 = 0;
   float* ItemAttVal;
   int CLSCNT = 10;
   int cdrf_result = 0;
   int Fcnt = 0;
   
   double recall_0 = 0;
   double recall_1 = 0;
   double recall_[10];
   double precision_[10];
   double precision_0 = 0;
   double precision_1 = 0;
   double recall_avg = 0;
   double precision_avg = 0;
   double F_measure_0 = 0;
   double F_measure_1 = 0;
   double F_measure_avg = 0;

   ItemAttVal = (float *) calloc(MaxAtt, sizeof(float));
   
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
	for(i = 0 ; i< 10; i++)
		{
		 	TT[i] = 0;
			FT[i] = 0;
			TF[i] = 0;
		}
	mid = (MaxItem - TEST)/2;
   	if(isCDRF_First == 1)
   	{
		for(n = 0 ; n< MaxItem ; n++)
   		{
   			for(i = 0; i<MaxAtt+1; i++)
   			{
				ItemAttVal[i] = CVal(Item[n],i);
   			}

			InterpretTree(Raw[0],ItemAttVal,0);
			if(oobval != Class(Item[n]))
				misscnt++;
			for(i =0; i<CLSCNT; i++)
			{
				if(Class(Item[n]) == i)
				{
					if(oobval == i)
						TT[i]++;
					for(j =0;j<CLSCNT;j++)
					{
						if(j==i) continue;
						else
						{
							if(oobval==j)
							{
								TF[i]++;
								FT[j]++;
							}
						}
					}
				}
			}
			
		}
		
		//printf("misscnt is %d\n",misscnt);
		totalcnt = MaxItem+1;
		truecnt = totalcnt - misscnt;
		recall = (double)truecnt/totalcnt;
		
		printf("TT[0] : %d, TT[1] : %d, TT[2] : %d\n",TT[0],TT[1],TT[2]);
		printf("TT[3] : %d, TT[4] : %d, TT[5] : %d\n",TT[3],TT[4],TT[5]);
		printf("TT[6] : %d, TT[7] : %d, TT[8] : %d, TT[9] : %d\n",TT[6],TT[7],TT[8],TT[9]);
		for(i=0;i<CLSCNT;i++)
		{
			recall_[i] = (double)TT[i]/(TT[i]+TF[i]);
			precision_[i] = (double)TT[i]/(TT[i]+FT[i]);
		}

		
		//F_measure_0 = (2*precision_0*recall_0) / (precision_0+recall_0);
		//F_measure_1 = (2*precision_1*recall_1) / (precision_1+recall_1);
		//F_measure_avg = (F_measure_0+F_measure_1) / 2; 
		//recall = F_measure_1;		
		//recall = F_measure_avg;
		/* Recall vs Precision covariance Test*/
		//Cov2[0][0] = 1;
		//Cov2[0][1] = recall;
		Cov2[0][0] = 1;
		Cov2[0][1] = 0; //precision
		Cov2[0][2] = 0; //recall
		Cov2[0][3] = 0; //F-measure
		for(i=0;i<CLSCNT;i++)
		{
		Cov2[0][1] = Cov2[0][1] + precision_[i];
		Cov2[0][2] = Cov2[0][2] + recall_[i];
		}
		Cov2[0][1] = Cov2[0][1]/CLSCNT;
		Cov2[0][2] = Cov2[0][2]/CLSCNT;
		//Cov2[0][3] = (2*Cov2[0][1]*Cov2[0][2]) / (Cov2[0][1]+Cov2[0][2]);
		Cov2[0][3] = 0;
		misscnt = 0;
		printf("F-measure is %f\n",Cov2[0][3]);

		fprintf(fp,"%d, %d, %f, %f, %f\n",Test_cnt+1, truecnt,Cov2[0][1],Cov2[0][2],Cov2[0][3]);
					
		Test_cnt = 1;
		isCDRF_First = 0;
   	}
	else
	{
		for(i = 0 ; i< 10; i++)
		{
		 	TT[i] = 0;
			FT[i] = 0;
			TF[i] = 0;
		}
		for(n = 0 ; n< MaxItem+1 ; n++)
   		{
   			for(i = 0; i<MaxAtt+1; i++)
   			{
				ItemAttVal[i] = CVal(Item[n],i);
   			}
			for(CDRFcnt =0 ;CDRFcnt <cnt+1 ; CDRFcnt++)
			{
				InterpretTree(Raw[CDRFcnt],ItemAttVal,0);
				if(oobval != Class(Item[n]))
				misscnt++;
				for(i =0; i<CLSCNT; i++)
			{
				if(Class(Item[n]) == i)
				{
					if(oobval == i)
						TT[i]++;
					for(j =0;j<CLSCNT;j++)
					{
						if(j==i) continue;
						else
						{
							if(oobval==j)
							{
								TF[i]++;
								FT[j]++;
							}
						}
					}
				}
			}
			}
		}

		totalcnt = MaxItem+1;
		truecnt = totalcnt - (misscnt/(cnt+1));

		recall = (double)truecnt/totalcnt;
		
		printf("TT[0] : %d, TT[1] : %d, TT[2] : %d\n",TT[0]/(cnt+1),TT[1]/(cnt+1),TT[2]/(cnt+1));
		printf("TT[3] : %d, TT[4] : %d, TT[5] : %d\n",TT[3]/(cnt+1),TT[4]/(cnt+1),TT[5]/(cnt+1));
		printf("TT[6] : %d, TT[7] : %d, TT[8] : %d, TT[9] : %d\n",TT[6]/(cnt+1),TT[7]/(cnt+1),TT[8]/(cnt+1),TT[9]/(cnt+1));
		for(i=0;i<CLSCNT;i++)
		{
			recall_[i] = (double)TT[i]/(TT[i]+TF[i]);
			precision_[i] = (double)TT[i]/(TT[i]+FT[i]);
		}

		//F_measure_0 = (2*precision_0*recall_0) / (precision_0+recall_0);
		//F_measure_1 = (2*precision_1*recall_1) / (precision_1+recall_1);
		//F_measure_avg = (F_measure_0+F_measure_1) / 2; 
		//recall = F_measure_1;
		//recall = F_measure_avg;
		
		/* CDRF F_measure */	
		//Cov2[Test_cnt][0] = cnt+1;
		//Cov2[Test_cnt][1] = recall;
		Cov2[Test_cnt][0] = cnt+1;
		Cov2[Test_cnt][1] = 0;
		Cov2[Test_cnt][2] = 0;
		Cov2[Test_cnt][3] = 0;
		for(i=0;i<CLSCNT;i++)
		{
		Cov2[Test_cnt][1] = Cov2[Test_cnt][1] + precision_[i];
		Cov2[Test_cnt][2] = Cov2[Test_cnt][2] + recall_[i];
		}
		Cov2[Test_cnt][1] = Cov2[Test_cnt][1]/CLSCNT;
		Cov2[Test_cnt][2] = Cov2[Test_cnt][2]/CLSCNT;
		Cov2[Test_cnt][3] = (2*Cov2[Test_cnt][1]*Cov2[Test_cnt][2]) / (Cov2[Test_cnt][1]+Cov2[Test_cnt][2]); //F-measure
		misscnt = 0;
		printf("F-measure[%d] is %f\n",Test_cnt,Cov2[Test_cnt][3]);

		if(Cov2[Test_cnt][3]-Cov2[Test_cnt-1][3] > 0 )		
		{
			if(iSF == 0)
			{
				Fcnt = Test_cnt-1;
				iSF = 1;
			}
		}

		Test_cnt++;

		if(iSF == 1)
{
		for(count = Fcnt ; count < Test_cnt; count++)
		{
			Ex = Ex + Cov2[count][0];
			Ey = Ey + Cov2[count][1];
			Ez = Ez + Cov2[count][2];
			Ev = Ev + Cov2[count][3];
		}
		
		Ex = Ex / (Test_cnt-Fcnt);
		Ey = Ey / (Test_cnt-Fcnt);	
		Ez = Ez / (Test_cnt-Fcnt);
		Ev = Ev / (Test_cnt-Fcnt);
}
		printf("Ex is %f, Ey is %f, Ez is %f.Ev is %f", Ex, Ey,Ez,Ev);
		if(iSF == 1)
{
		for(count = Fcnt ; count < Test_cnt; count++)
		{
			recall_x = Cov2[count][0] - Ex;
			recall_y = Cov2[count][1] - Ey;
			recall_z = Cov2[count][2] - Ez;
			recall_v = Cov2[count][3] - Ev;
			result = result + (recall_x*recall_y);
			result2 = result2 + (recall_x*recall_z);
			result3 = result3 + (recall_x*recall_v);
		}
		result = result / (Test_cnt-Fcnt);
		result2 = result2 / (Test_cnt-Fcnt);
		result3 = result3 / (Test_cnt-Fcnt);
		covariance[Test_cnt-2] = result3;
		//fprintf(fp,"F_measure : %f, %f \n",F_measure_0, F_measure_1);
		fprintf(fp,"%d, %d, %f, %f, %f, ",Test_cnt, truecnt,Cov2[Test_cnt-1][1],Cov2[Test_cnt-1][2],Cov2[Test_cnt-1][3]);
		fprintf(fp,"%f, %f, %f\n",result,result2,result3);
}		
		if(covariance[Test_cnt-2]-covariance[Test_cnt-3] < 0)
			cdrf_result = 1;		
		//printf("result is %f\n",result);
		result = 0;
		result2 = 0;
		result3 = 0;
		Ex = 0;
		Ey = 0;	
	}
	
	//free(ItemAttVal);
	//free(RangeDesc);
	return cdrf_result;
}
int MCnemar(Tree* Raw, int cnt)
{
   int i,n,a,count,j;
   int misscnt = 0;
   int truecnt = 0;
   int totalcnt = 0;
   double recall = 0;
   double Fmeasure = 0;
   int mid = 0;
 
   int MCcnt =0;
      
   float* ItemAttVal;
   int mc_result = 0;

   int TTcnt = 0;
   int TFcnt = 0;
   int FTcnt = 0;
   int FFcnt = 0;

   ItemAttVal = (float *) calloc(MaxAtt, sizeof(float));
   
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
   	if(isMC_First == 1)
   	{
		//for(n = 0 ; n< MaxItem-TOTAL ; n++)
		for(n = 0 ; n< MaxItem ; n++)
   		{
   			for(i = 0; i<MaxAtt+1; i++)
   			{
				//ItemAttVal[i] = CVal(MCnemarItem[n],i);
				ItemAttVal[i] = CVal(Item[n],i);
   			}

			for(MCcnt =0 ;MCcnt <cnt+1 ; MCcnt++)
			{
				InterpretTree(Raw[MCcnt],ItemAttVal,0);
				//if(oobval != Class(MCnemarItem[n]))
				if(oobval != Class(Item[n]))
				{
					misscnt++;
				}
				else
				{
					truecnt++;			
				}
			}
			if(misscnt >= truecnt)		
			{
				BEFORE[n] = 0;//false
				misscnt = 0;
				truecnt = 0;
			}
			else if(truecnt > misscnt)
			{
				BEFORE[n] = 1;//false
				misscnt = 0;
				truecnt = 0;
			}
		}
		printf("FIRST TREE IS COMPLETED !!!\n");
	
		Test_cnt = 2;
		isMC_First = 0;
   	}
	else
	{
		for(n = 0 ; n< MaxItem; n++)
   		{
   			for(i = 0; i<MaxAtt+1; i++)
   			{
				//ItemAttVal[i] = CVal(MCnemarItem[n],i);
				ItemAttVal[i] = CVal(Item[n],i);
   			}
			for(MCcnt =0 ;MCcnt <cnt+1 ; MCcnt++)
			{
				InterpretTree(Raw[MCcnt],ItemAttVal,0);
				//if(oobval != Class(MCnemarItem[n]))
				if(oobval != Class(Item[n]))
				{
					misscnt++;
				}
				else
				{
					truecnt++;			
				}
			}
			if(misscnt >= truecnt)		
			{
				AFTER[n] = 0;//false
				misscnt = 0;
				truecnt = 0;
			}
			else if(truecnt > misscnt)
			{
				AFTER[n] = 1;//false
				misscnt = 0;
				truecnt = 0;
			}

			//printf("%d ",n);
		}
		
		//for(n = 0 ; n< MaxItem-TOTAL ; n++)
		for(n = 0 ; n< MaxItem; n++)
		{
			if(BEFORE[n] == 1 && AFTER[n] == 1)
				TTcnt++;
			else if(BEFORE[n] == 1 && AFTER[n] == 0)
				TFcnt++;
			else if(BEFORE[n] == 0 && AFTER[n] == 1)
				FTcnt++;
			else if(BEFORE[n] == 0 && AFTER[n] == 0)
				FFcnt++;
			BEFORE[n] = AFTER[n];
		}
		Test_cnt++;
		printf("%d TREE IS COMPLETED !!!\n",Test_cnt-1);
		fprintf(fp,"%d, %d, %d, %d, %d\n",Test_cnt-2, TTcnt,TFcnt, FTcnt, FFcnt);
	}

	return mc_result;
}

