#include <stdio.h>
#include <string.h>

#define MAX_SIZE 10000


unsigned char double_sum[10] = {0, 2, 4, 6, 8, 1, 3, 5, 7, 9};


// This uses 3 pointers to scan the target string in place
// The first (i) is moving forward till it finds a number
// That triggers the inner loop that now runs forward
// Looking for something not a number (or space/dash)
int luhny(unsigned char * numbers, int start, int finish)
{
    int i;
    int total=0;
    int odd=1;
    
    for(i = finish-1; i >= start; --i)
    {
        if(numbers[i] < 10)
        {
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
    
    return (total % 10) == 0;
}

void process(unsigned char *numbers)
{
    int i;
    int j;
    int k;

    for(i=0; numbers[i] != 208; ++i)
    {
        // Inner loop to process any number string found
        if(numbers[i] < 10)
        {
            for(j = i+1;
                numbers[j] <  10  ||  // IS IT A NUMBER
                numbers[j] == 240 ||  // IS IT A SPACE
                numbers[j] == 253;    // IS IT A DASH
                ++j)
            {
            } // Keep spinning forward
                
            if(luhny(numbers, i, j))
            {
                for(k=i; k<j; ++k) numbers[k] = 40; // 'X'
            }

            i = j; // Continue the search
        }
    }
}

// This takes the string and rotates by '0', such that the
// ASCII zero is zero. Of course, NULL is now 208.
// This relies on the machine size of char being 8bits, which
// in general is not true, but for most common hardware is true.
void process_line(unsigned char *str)
{
    int i;
    
    // Roll the whole buffer around to '0' = > 0
    for (i = 0; str[i] != 0; ++i) str[i] -= '0';
    str[i] -= '0';
    
    process(str);
    
    // Roll it back, pretend nothing happened
    for (i = 0; str[i] != 208; ++i) str[i] += '0';
    str[i] = 0;
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