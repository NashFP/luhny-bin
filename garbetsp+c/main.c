#include <stdio.h>
#include <string.h>

#define MAX_SIZE 10000

// This is far from "functional", but more of an attempt to 
// see just how fast C can do it.
// Key features, buffer is modified in place
// First, flipped into number space
// Thus comparison for number are a single less than.
// One big loop does most of the work
// 2*number add digits, is done via a look-up table
// luhny routine modifies buffer directly as one giant side-effect

unsigned char double_sum[10] = {0, 2, 4, 6, 8, 1, 3, 5, 7, 9};


// This uses 3 pointers to scan the target string in place
// The first (i) is moving forward till it finds a number
// That triggers the inner loop that now runs forward
// Looking for something not a number (or space/dash)
int luhny(unsigned char * numbers, int start, int finish)
{
    int i;
    int total=0;
    int odd=0;
    int len=0;
    
    // Compute length in digits, and Luhn total
    for(i = finish-1; i >= start; --i)
    {
        if(numbers[i] < 10)
        {
            ++len;
            if(odd)
            {
                total += double_sum[numbers[i]];
                odd=0;
            }
            else
            {
                total += numbers[i];
                odd=1;
            }
        }
    }
    
    // If the digit length is greater than 16, recurse with two
    // subsets, 1 shorter.
    if(len > 16)
    {
        return luhny(numbers, start+1, finish) || 
               luhny(numbers, start, finish-1);
    }
    else if (len >= 14) // If it's long enough continue
    {
        if((total % 10) == 0) // Is it Luhn-y
        {
            // Mark that out in the buffer
            for(i = finish-1; i >= start; --i) numbers[i] = 40;
            return 1;
        }
        else if(len > 14) // If it's still long enough, consider sub-strings
        {
            return luhny(numbers, start+1, finish) || luhny(numbers, start, finish-1);
        }
    }
    
    return 0;
}

// This takes the string and rotates by '0', such that the
// ASCII zero is zero. Of course, NULL is now 208.
// This relies on the machine size of char being 8bits, which
// in general is not true, but for most common hardware is true.
void process_line(unsigned char *str)
{
    int i;
    int j;
    int k;
    
    // Roll the whole buffer around to '0' = > 0
    for (i = 0; str[i] != 0; ++i)
    {
        str[i] -= '0'; // Flip into unsigned byte space

        if(str[i] < 10)
        {
            for(j = i+1; str[j] != 0; ++j)
            {
                str[j] -= '0';
    
                if(str[j] > 9 && str[j] != 240 && str[j] != 253) break;
            } // Keep spinning forward
            
            // Got a numeric sequence starting with a number, and followed by 
            // spaces, dashes and numbers, now process
            luhny(str, i, j);
            
            // Flip out of unsigned byte space
            for(k=i; k<j; ++k) str[k] += '0';

            i = j; // Continue the search
        }

        // Uninteresting characters, flip back out of byte space
        str[i] += '0';
    }  
}

// Preallocate buffer when program loads
static unsigned char buffer[MAX_SIZE];

int main (int argc, char *argv[])
{
    while(fgets(buffer, MAX_SIZE, stdin) != NULL)
    {
        process_line(buffer);
        printf("%s",(char *)buffer);
    }
    
    return 0;
}