#include <stdio.h>
#include <stdint.h>

const int false = 0;
const int true = 1;
const int bottom = 2;
const int top = 4;


uint8_t isConstant(uint8_t b)
{
    return b < 2;
}

/**
 * @brief 
 * if b is true 
 *      0 0 1
 * &    0 0 1
 * =    0 0 1
 * if b is false
 *      0 0 0
 * &    0 0 1
 * =    0 0 0
 * 
 * if b is top
 *      1 0 0
 * >> 2 
 * =    0 0 1
 * 
 * if b is bottom
 *      0 1 0
 * >> 2
 *      0 0 0 
 */
uint8_t boolIsTrue(uint8_t b)
{
    if(isConstant(b)) {
        return b & 1;
    } 
    return b >> 2;
    
}

uint8_t boolIsFalse(uint8_t b)
{
    if(isConstant(b)) {
        return !b;
    }
    return b >> 2;  
}

uint8_t boolNot(uint8_t b)
{
    if(isConstant(b)) {
        return !b;
    }
    return b;
}

uint8_t boolJoin(uint8_t a, uint8_t b)
{
    // either a or b is top
    if((a | b) >> 2) {
        return top;
    }
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
    // a and b are both constants
    return a == b ? a : top;
}

uint8_t boolMeet(uint8_t a, uint8_t  b) {
    if(a == top) {
        return b;
    }
    if(b == top) {
        return a;
    }
    // either a or b are bottom
    if((a | b) >> 1) {
        return bottom;
    }
    return a == b ? a : bottom;
}

uint8_t boolSubsumes(uint8_t a, uint8_t b) {
    if(a == top || b == bottom) {
        return true;
    }
    if(b == top) {
        return false;
    }
    return isConstant(a) ? a == b : false;
}