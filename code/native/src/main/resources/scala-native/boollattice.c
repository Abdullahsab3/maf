#include <stdio.h>
#include <stdint.h>

const uint8_t false = 0;
const uint8_t true = 1;
const uint8_t bottom = 2;
const uint8_t top = 3;


uint8_t isConstant(uint8_t b)
{
    return b < 2;
}

uint8_t isTrue(uint8_t b)
{
    return b & 1;
}

uint8_t isFalse(uint8_t b)
{
    return isConstant(b) ? !b : b & 1;
}

uint8_t not(uint8_t b)
{
    return isConstant(b) ? !b : b;
}

uint8_t boolsjoin(uint8_t a, uint8_t b)
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

uint8_t boolsmeet(uint8_t a, uint8_t  b) {
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

uint8_t boolssubsumes(uint8_t a, uint8_t b) {
    if(a == top || b == bottom) {
        return true;
    }
    if(b == top) {
        return false;
    }
    return isConstant(a) ? a == b : false;
}

char *showBool(uint8_t b) {
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


