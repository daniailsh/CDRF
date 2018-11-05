/*************************************************************************/
/*								   	 */
/*	Classify items interactively using a decision tree	   	 */
/*	--------------------------------------------------   		 */
/*								   	 */
/*************************************************************************/


#include "defns.i"
//#include "extern.i"
#include "types.i"
#include <stdlib.h>
#define	SkipSpace	while ( (c = getchar()) == ' ' || c == '\t' )
		
short		MaxAtt, MaxClass, MaxDiscrVal;

ItemNo		MaxItem;

Description	*Item;

DiscrValue	*MaxAttVal;

String		*ClassName,
		*AttName,
		**AttValName;
extern String 	FileName;

char		*SpecialStatus;

extern short	VERBOSITY;
short		TRACE     = 0;
Tree	*Pruned;
int	oobval;

int represent;
	/*  The interview module uses a more complex description of an
	    case called a "Range Description".   The value of an
	    attribute is given by
	    - lower and upper bounds (continuous attribute)
	    - probability of each possible value (discrete attribute)  */

int Nomal;
int Anomaly;
int OK_dev[10];
char class[4];
int cnt;
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

Tree	DecisionTree,			/* tree being used */
	GetTree();

float	*LowClassSum,			/* accumulated lower estimates */
	*ClassSum = Nil;		/* accumulated central estimates */

#define Fuzz	0.01			/* minimum weight */



/*************************************************************************/
/*								   	 */
/*  Classify the extended case description in RangeDesc using the	 */
/*  given subtree, by adjusting the values ClassSum and LowClassSum	 */
/*  for each class, indicating the likelihood of the case being  	 */
/*  of that class.						   	 */
/*								   	 */
/*************************************************************************/


    ClassifyCase(Subtree, Weight, test_value, flag)
/*  ------------ 	 */
    Tree Subtree;
    float Weight;
    float* test_value;
    int flag;
{
    DiscrValue v;
    float BranchWeight, Area(), Interpolate();
    Attribute a;
    short s;
    ClassNo c;
    int i;
    /*  A leaf  */
	
    if ( ! Subtree->NodeType )
    {

	Verbosity(1)
	    printf("\tClass %s weight %g cases %g\n", 
		    ClassName[Subtree->Leaf], Weight, Subtree->Items);

	if ( Subtree->Items > 0 )
	{
	    /*  Adjust class sum of ALL classes, but adjust low class sum
		of leaf class only  */
	
	    ForEach(c, 0, MaxClass)
	    {
		ClassSum[c] += Weight * Subtree->ClassDist[c] / Subtree->Items;
	    }

	    LowClassSum[Subtree->Leaf]  += Weight *(1 - Subtree->Errors / Subtree->Items);
	}
	else
	{
	    ClassSum[Subtree->Leaf] += Weight;
	}

	return;
    }
	
    a = Subtree->Tested;
        
    CheckValue(a, Subtree, test_value, flag);

    /*  Unknown value  */

    if ( ! RangeDesc[a].Known )
    {
	ForEach(v, 1, Subtree->Forks)
	{
	    ClassifyCase(Subtree->Branch[v],
		     (Weight * Subtree->Branch[v]->Items) / Subtree->Items, test_value, flag);
	}
	return;
    }

    /*  Known value  */

   switch ( Subtree->NodeType )
    {
	case BrDiscr:  //test of discrete attribute 

	    ForEach(v, 1, MaxAttVal[a])
	    {
		BranchWeight = RangeDesc[a].Probability[v];
		if ( BranchWeight > 0 )
		{
		    Verbosity(1)
		    	printf("\tWeight %g: test att %s (val %s = %g)\n",
		    	       Weight, AttName[a], AttValName[a][v],
			       BranchWeight);

		    ClassifyCase(Subtree->Branch[v], Weight * BranchWeight, test_value, flag);
		}
	    }
	    break;

	case ThreshContin:  // test of continuous attribute 

	    BranchWeight = 
		RangeDesc[a].UpperBound <= Subtree->Lower ? 1.0 :
		RangeDesc[a].LowerBound > Subtree->Upper ? 0.0 :
		RangeDesc[a].LowerBound != RangeDesc[a].UpperBound ?
		    (Area(Subtree, RangeDesc[a].LowerBound) -
		     Area(Subtree, RangeDesc[a].UpperBound)) /
		    (RangeDesc[a].UpperBound - RangeDesc[a].LowerBound) :
		Interpolate(Subtree, RangeDesc[a].LowerBound) ;

	    Verbosity(1)
	        printf("\tWeight %g: test att %s (branch weight=%g)\n",
	    	       Weight, AttName[a], BranchWeight);

	    if ( BranchWeight > Fuzz )
	    {
		ClassifyCase(Subtree->Branch[1], Weight * BranchWeight, test_value, flag);
	    }
	    if ( BranchWeight < 1-Fuzz )
	    {
		ClassifyCase(Subtree->Branch[2], Weight * (1 - BranchWeight), test_value,flag);
	    }
	    break;

	case BrSubset:  // subset test on discrete attribute 

	    ForEach(s, 1, Subtree->Forks)
	    {
		BranchWeight = 0.0;
		ForEach(v, 1, MaxAttVal[a])
		{
		    if ( In(v, Subtree->Subset[s]) )
		    {
			BranchWeight += RangeDesc[a].Probability[v];
		    }
		}
		if ( BranchWeight > 0 )
		{
		    Verbosity(1)
		    	printf("\tWeight %g: test att %s (val %s = %g)\n",
		    	       Weight, AttName[a], AttValName[a][v],
			       BranchWeight);

		    ClassifyCase(Subtree->Branch[s], Weight * BranchWeight, test_value, flag);
		}
	    }
	    break;
    }
}



/*************************************************************************/
/*								   	 */
/*  Interpolate a single value between Lower, Cut and Upper		 */
/*								   	 */
/*************************************************************************/


float Interpolate(t, v)
/*    ---- 	 */
    Tree t;
    float v;
{
    float Sum=Epsilon;

    if ( v <= t->Lower )
    {
	return 1.0;
    }

    if ( v <= t->Cut )
    {
	return 1 - 0.5 * (v - t->Lower) / (t->Cut - t->Lower + Epsilon);
    }

    if ( v < t->Upper )
    {
	return 0.5 - 0.5 * (v - t->Cut) / (t->Upper - t->Cut + Epsilon);
    }

    return 0.0;
}



/*************************************************************************/
/*								   	 */
/*  Compute the area under a soft threshold curve to the right of a	 */
/*  given value.							 */
/*								   	 */
/*************************************************************************/


float Area(t, v)
/*    ---- 	 */
    Tree t;
    float v;
{
    float Sum=Epsilon, F;

    if ( v < t->Lower )
    {
	Sum += t->Lower - v;
	v = t->Lower;
    }

    if ( v < t->Cut )
    {
	F = (t->Cut - v ) / (t->Cut - t->Lower + Epsilon);

	Sum += 0.5 * (t->Cut - v) + 0.25 * F * (t->Cut - v);
	v = t->Cut;
    }

    if ( v < t->Upper )
    {
	F = (t->Upper - v ) / (t->Upper - t->Cut + Epsilon);

	Sum += 0.25 * (t->Upper - v) * F;
    }

    Verbosity(1) printf("lower=%g  cut=%g  upper=%g  area=%g\n",
    			t->Lower, t->Cut, t->Upper, Sum);

    return Sum;
}



/*************************************************************************/
/*								  	 */
/*		Process a single case				  	 */
/*								  	 */
/*************************************************************************/


    InterpretTree(Decision_Tree, test_value, flag)
/*  ------------- 	 */
    Tree Decision_Tree;
    float* test_value;
    int flag;
{ 
    ClassNo c, BestClass;
    float Uncertainty=1.0;
    char Reply;
    Attribute a;
    float BranchWeight, Area(), Interpolate();
    int i;
    represent = 0;
    /*  Initialise  */

    ForEach(a, 0, MaxAtt)
    {
	RangeDesc[a].Asked = false;
    }
    
    if ( ! ClassSum )
    {
	/*  The first time through .. allocate class sums  */

	ClassSum = (float *) malloc((MaxClass+1) * sizeof(float));
	LowClassSum = (float *) malloc((MaxClass+1) * sizeof(float));
	
	if(flag == 1) printf("\n");
    }
    else
    {
	;//if(flag == 1) printf("\n-------------------------------------------\n\n");
    }

    ForEach(c, 0, MaxClass)
    {
	LowClassSum[c] = ClassSum[c] = 0;
    }

    /*  Find the likelihood of an item's being of each class  */
  
    // PrintTree(Decision_Tree);
    ClassifyCase(Decision_Tree, 1.0, test_value, flag);	   					 /// many time gogo
    /*  Find the best class and show decision made  */

    BestClass = 0;
    ForEach(c, 0, MaxClass)
    {
	Verbosity(1) printf("class %d weight %.2f\n", c, ClassSum[c]);

	Uncertainty -= LowClassSum[c];
	if ( ClassSum[c] > ClassSum[BestClass] ) BestClass = c;
    }

    //if(flag == 1) printf("\nDecision:\n");
    Decision(BestClass, ClassSum[BestClass],
	     LowClassSum[BestClass],
	     Uncertainty + LowClassSum[BestClass],flag);
	represent++;	
    /*  Show the other significant classes, if more than two classes  */

    if ( MaxClass > 1 )
    {
	while ( true )
	{
	    ClassSum[BestClass] = 0;
	    BestClass = 0;
	    ForEach(c, 0, MaxClass)
	    {
		if ( ClassSum[c] > ClassSum[BestClass] ) BestClass = c;
	    }

	    if ( ClassSum[BestClass] < Fuzz ) break;

	    Decision(BestClass, ClassSum[BestClass],
		     LowClassSum[BestClass],
		     Uncertainty + LowClassSum[BestClass], flag);
	}
    }
}



/*************************************************************************/
/*								  	 */
/*  Print the chosen class with certainty factor and range	  	 */
/*								  	 */
/*************************************************************************/


    Decision(c, p, lb, ub, flag)
/*  -------- 	 */
    ClassNo c;
    float p, lb, ub;
    int flag;
{
    if(flag == 0 )
    {
	
	/*for(cnt=0;cnt<10;cnt++)
	{
		snprintf(class,sizeof(int),"%d",cnt);
		if(strcmp(ClassName[c],class)==0)
			oobval = cnt;
	}	*/
	if(strcmp(ClassName[c],"0")==0)
		oobval = 0;
	else if(strcmp(ClassName[c],"1")==0)
		oobval = 1;
	else if(strcmp(ClassName[c],"2")==0)
		oobval = 2;
	else if(strcmp(ClassName[c],"3")==0)
		oobval = 3;
	else if(strcmp(ClassName[c],"4")==0)
		oobval = 4;
	else if(strcmp(ClassName[c],"5")==0)
		oobval = 5;
	else if(strcmp(ClassName[c],"6")==0)
		oobval = 6;	
	else if(strcmp(ClassName[c],"7")==0)
		oobval = 7;	
	else if(strcmp(ClassName[c],"8")==0)
		oobval = 8;	
	else if(strcmp(ClassName[c],"9")==0)
		oobval = 9;	
	
	/*if(strcmp(ClassName[c],"E")==0)
		oobval = 0;
   	else if(strcmp(ClassName[c],"F")==0)
		oobval = 1; 
   	else if(strcmp(ClassName[c],"G")==0)
		oobval = 2;
 	*/
    }
    else if(flag == 1) 
    {
	//printf("HERE!!!\n");
	//printf("\t%s", ClassName[c]);
	if(represent ==0)
	{
	/*for(cnt=0;cnt<10;cnt++)
	{
		snprintf(class,sizeof(int),"%d",cnt);
		if(strcmp(ClassName[c],class)==0)
			OK_dev[cnt]++;
		//class = "";
	}*/

	
	if(strcmp(ClassName[c],"0")==0)
		OK_dev[0]++;
	else if(strcmp(ClassName[c],"1")==0)
		OK_dev[1]++;
	else if(strcmp(ClassName[c],"2")==0)
		OK_dev[2]++;
	else if(strcmp(ClassName[c],"3")==0)
		OK_dev[3]++;
	else if(strcmp(ClassName[c],"4")==0)
		OK_dev[4]++;
	else if(strcmp(ClassName[c],"5")==0)
		OK_dev[5]++;
	else if(strcmp(ClassName[c],"6")==0)
		OK_dev[6]++;	
	else if(strcmp(ClassName[c],"7")==0)
		OK_dev[7]++;	
	else if(strcmp(ClassName[c],"8")==0)
		OK_dev[8]++;	
	else if(strcmp(ClassName[c],"9")==0)
		OK_dev[9]++;	
	
   	}
	
    	/*if ( p < 1-Fuzz || lb < ub - Fuzz )
    	{
		if(flag == 1) printf("  CF = %.2f", p);
		if ( lb < ub - Fuzz )
		{
	    		if(flag == 1) printf("  [ %.2f - %.2f ]", lb, ub);
		}
    	}

   	 printf("\n");*/
    }


}

