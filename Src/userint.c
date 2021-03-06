/*************************************************************************/
/*								 	 */
/*	User interface for consulting trees and rulesets	 	 */
/*	------------------------------------------------	 	 */
/*								 	 */
/*************************************************************************/


#include "defns.i"
#include "types.i"
#include "extern.i"

short		MaxAtt;

typedef	struct	ValRange	*RangeDescRec;
struct	ValRange
	{
	    Boolean		Known, Asked;
	    float		LowerBound, UpperBound, *Probability;
	};

extern	RangeDescRec		RangeDesc;


#define Fuzz 0.01


/*************************************************************************/
/*									 */
/*	Ask for the value of attribute Att if necessary			 */
/*									 */
/*************************************************************************/


    CheckValue(Att, T, test_value, flag)
/*  ----------  */
    Attribute Att;
    Tree T;
    float* test_value;
    int flag;
{
    if ( RangeDesc[Att].Asked ) return;
//Please modify it
   // if(flag == 1) printf("%s", AttName[Att]);
 /*   if ( RangeDesc[Att].Known )
    {
	printf(" [ ");
	PrintRange(Att);
	printf(" ]");
    }
 */
  // if(flag == 1)  printf(": ");

    ReadRange(Att, T, test_value, flag);
}


 
/*************************************************************************/
/*									 */
/*	Print the range of values for attribute Att			 */
/*									 */
/*************************************************************************/


    PrintRange(Att)
/*  -----------  */
    Attribute Att;
{
    DiscrValue dv;
    Boolean First=true;
    float p;

    if ( MaxAttVal[Att] )  /*  discrete attribute  */
    {
	ForEach(dv, 1, MaxAttVal[Att] )
	{
	    if ( (p = RangeDesc[Att].Probability[dv]) > Fuzz )
	    {
		if ( ! First )
		{
		    printf(", ");
		}
		First = false;

		printf("%s", AttValName[Att][dv]);
		if ( p < 1-Fuzz )
		{
		    printf(": %.2f", p);
		}
	    }
	}
    }
    else  /*  continuous attribute  */
    {
	printf("%g", RangeDesc[Att].LowerBound);
	if ( RangeDesc[Att].UpperBound > RangeDesc[Att].LowerBound + Fuzz )
	{
	    printf(" - %g", RangeDesc[Att].UpperBound);
	}
    }
}



extern	char		Delimiter;
#define	SkipSpace	while ( (c = getchar()) == ' ' || c == '\t' )


/*************************************************************************/
/*									 */
/*	Read a range of values for attribute Att or <cr>		 */
/*									 */
/*************************************************************************/


    ReadRange(Att, T, test_value, flag)
/*  ----------  */
    Attribute Att;
    Tree T;
    float* test_value;
    int flag;
{
    char c;
    //printf("Att value : %d\n ", Att);

    RangeDesc[Att].Asked=true;

    //SkipSpace;

    if ( c == '\n' )
    {
	return;
    }
    if ( c == '?' )
    {
	if ( (c = getchar()) == 't' )
	{
	    if ( T ) PrintTree(T);
	    SkipLine(c);
	    RangeDesc[Att].Asked = false;
	    CheckValue(Att, T, test_value, flag);
	}
	else
	{
	    RangeDesc[Att].Known = false;
	    SkipLine(c);
	}
	return;
    }
	
    ungetc(c, stdin);
    RangeDesc[Att].Known = true;

    if ( MaxAttVal[Att] )
    {
	ReadDiscr(Att, T, test_value, flag);
    }
    else
    { //Continuous Data Read
	ReadContin(Att, T, test_value, flag);
    }
}



/*************************************************************************/
/*									 */
/*	Read a discrete attribute value or range			 */
/*									 */
/*************************************************************************/

    ReadDiscr(Att, T, test_value, flag)
/*  ---------  */
    Attribute Att;
    Tree T;
    float* test_value;
    int flag;
{
    char Name[500];
    Boolean ReadName();
    DiscrValue dv, PNo;
    float P, PSum;

    ForEach(dv, 1, MaxAttVal[Att])
    {
	RangeDesc[Att].Probability[dv] = 0.0;
    }

    do
    {
	ReadName(stdin, Name);

	dv = Which(Name, AttValName[Att], 1, MaxAttVal[Att]);
	if ( ! dv )
	{
	    printf("\tPermissible values are %s", AttValName[Att][1]);
	    ForEach(dv, 2, MaxAttVal[Att])
	    {
		printf(", %s", AttValName[Att][dv]);
	    }
	    printf("\n");

	    SkipLine(Delimiter);
	    Retry(Att, T, test_value, flag);
	    return;
	}

	if ( Delimiter == ':' )
	{
	    ReadName(stdin, Name);
	    sscanf(Name, "%f", &P);	/* get probability */
	}
	else
	{
	    P = 1.0;		/*  only one attribute value  */
	}

	RangeDesc[Att].Probability[dv] = P;
    }
    while ( Delimiter == ',' );

    /*  Check that sum of probabilities is not > 1  */

    PNo = MaxAttVal[Att];
    PSum = 1.0;
    ForEach(dv, 1, MaxAttVal[Att])
    {
	if ( RangeDesc[Att].Probability[dv] > Fuzz )
	{
	    PSum -= RangeDesc[Att].Probability[dv];
	    PNo--;
	}
    }

    if ( PSum < 0 || ! PNo && PSum > Fuzz )
    {
	printf("Probability values must sum to 1\n");
	SkipLine(Delimiter);
	Retry(Att, T, test_value, flag);
	return;
    }

    /*  Distribute the remaining probability equally among
	the unspecified attribute values  */

    PSum /= PNo;
    ForEach(dv, 1, MaxAttVal[Att])
    {
	if ( RangeDesc[Att].Probability[dv] < Fuzz )
	{
	    RangeDesc[Att].Probability[dv] = PSum;
	}
    }
}



/*************************************************************************/
/*									 */
/*	Read a continuous attribute value or range			 */
/*									 */
/*************************************************************************/


    ReadContin(Att, T, test_value, flag)
/*  ----------  */
    Attribute Att;
    Tree T;
    float* test_value;
    int flag;
{
    int i=0;
    char c='\n';
    
    for(i=0;i<MaxAtt+1;i++)
    {
	if(Att==i)
	{
		RangeDesc[Att].LowerBound = test_value[i];
		//if(flag ==1) printf("%.2f\n",RangeDesc[Att].LowerBound);
	}

    }
/*    if(Att==0)
    {
 	RangeDesc[Att].LowerBound = 100;
	//printf("%.2f\n",RangeDesc[Att].LowerBound);
	printf("%.2f\n",test_value[0]);
    }
    else if(Att==1)
    {
	RangeDesc[Att].LowerBound = 10;
	//printf("%.2f\n",RangeDesc[Att].LowerBound);
	printf("%.2f\n",test_value[1]);
    }
    else
    {
	RangeDesc[Att].LowerBound = 10;
	//printf("%.2f\n",RangeDesc[Att].LowerBound);
	printf("%.2f\n",test_value[2]);
    }
*/
    //scanf("%f", &RangeDesc[Att].LowerBound);
    //SkipSpace;

    if ( c == '-'  )
    {
	scanf("%f", &RangeDesc[Att].UpperBound);
	SkipSpace;
    }
    else
    {
	RangeDesc[Att].UpperBound = RangeDesc[Att].LowerBound;
    }

    if ( c != '\n' )
    {
	printf("Must be a continuous value or range\n");
	SkipLine(c);
	Retry(Att, T, test_value, flag);
    }
}



/*************************************************************************/
/*									 */
/*	Try again to obtain a value for attribute Att			 */
/*									 */
/*************************************************************************/

    Retry(Att, T, test_value, flag)
/*  -----  */
    Attribute Att;
    Tree T;
    float* test_value;
    int flag;
{
    RangeDesc[Att].Asked = false;
    RangeDesc[Att].Known = false;
    CheckValue(Att, T, test_value, flag);
}



/*************************************************************************/
/*									 */
/*	Skip to the end of the line of input				 */
/*									 */
/*************************************************************************/


    SkipLine(c)
/*  --------  */
    char c;
{
    while ( c != '\n' ) c = getchar();
}



/*************************************************************************/
/*									 */
/*		Clear the range description				 */
/*									 */
/*************************************************************************/

    Clear()
/*  -----  */
{
    Attribute Att;

    ForEach(Att, 0, MaxAtt)
    {
	RangeDesc[Att].Known = false;
    }
}
