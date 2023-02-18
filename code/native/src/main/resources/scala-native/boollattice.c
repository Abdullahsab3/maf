#include <stdio.h>
#include <stdint.h>

const int false = 0;
const int true = 1;
const int bottom = 2;
const int top = 3;


int isConstant(int b)
{
    return b < 2;
}

int boolIsTrue(int b)
{
    return b & 1;
}

int boolIsFalse(int b)
{
    return isConstant(b) ? !b : b & 1;
}

int boolNot(int b)
{
    return isConstant(b) ? !b : b;
}

int boolJoin(int a, int b)
{
    // a is bottom element
    if (a == bottom)
    {
        return b;
    }
    // b is bottom element
    if (b == bottom)
    {
        return a;
    }

    // Either one of the elements is the top element
    // This works becaus we already checked whether one of them is bottom element (i.e., 0000 0010)
    // So, if the third rightmost bit is 1 now, we have a top element
    if ((a | b) >> 1)
    {
        return top;
    }
    // a and b are both constants
    return a == b ? a : top;
}

int boolMeet(int a, int  b) {
    if(a == top) {
        return b;
    }
    if(b == top) {
        return a;
    }

    if((a | b) >> 1) {
        return bottom;
    }

    return a == b ? a : bottom;
}

int boolSubsumes(int a, int b) {
    if(a == top || b == bottom) {
        return true;
    }
    if(b == top) {
        return false;
    }
    return isConstant(a) ? a == b : false;
}

char *showBool(int b) {
    switch(b) {
        case true:
        return "true";
        case false:
        return "false";
        case top:
        return "Boolean";
        case bottom:
        return "Boolean.⊥";
    }
    return "unknownType";
}






/* const int false = 0;
const int true = 1;
const int bottom = 2;
const int top = 3;


int isConstant(int b)
{
    return b < 2;
}

int isTrue(int b)
{
    return b & 1;
}

int isFalse(int b)
{
    return isConstant(b) ? !b : b & 1;
}

int not(int b)
{
    return isConstant(b) ? !b : b;
}

int boolsjoin(int a, int b)
{
    // a is bottom element
    if (a == bottom)
    {
        return b;
    }
    // b is bottom element
    if (b == bottom)
    {
        return a;
    }

    // Either one of the elements is the top element
    // This works becaus we already checked whether one of them is bottom element (i.e., 0000 0010)
    // So, if the third rightmost bit is 1 now, we have a top element
    if ((a | b) >> 1)
    {
        return top;
    }
    // a and b are both constants
    return a == b ? a : top;
}

int boolsmeet(int a, int  b) {
    if(a == top) {
        return b;
    }
    if(b == top) {
        return a;
    }

    if((a | b) >> 1) {
        return bottom;
    }

    return a == b ? a : bottom;
}

int boolssubsumes(int a, int b) {
    if(a == top || b == bottom) {
        return true;
    }
    if(b == top) {
        return false;
    }
    return isConstant(a) ? a == b : false;
}

char *showBool(int b) {
    switch(b) {
        case true:
        return "true";
        case false:
        return "false";
        case top:
        return "Boolean";
        case bottom:
        return "Boolean.⊥";
    }
    return "unknownType";
}


 */