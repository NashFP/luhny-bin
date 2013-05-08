#include <stdio.h>
#include <string.h>

#define MAX_SIZE 10000


typedef enum {
	FALSE,
	TRUE
} boolean;


boolean is_number (const int x)
{
	if (x >= 0 && x < 10)
	{
		return TRUE;
	}
	return FALSE;
}


int get_number_of_digits (const int *const numbers, const int size)
{
	int count = 0;	
	int i;

	for (i = 0; i < size; ++i)
	{
		if (is_number (numbers[i]))
		{
			++count;
		}
	}
	return count;
}


int try_add_double (const int number)
{
	int doubled = number * 2;
	
	if (doubled >= 10)
	{
		return 1 + (doubled - 10);
	}
	return doubled;
}



void _double (int *const numbers, const int size, const int remainder, int *const nums)
{
	int i;

	for (i = 0; i < size; ++i)
	{
		const int x = i % 2;
		
		if ((x == 0 && remainder == 0) || (x != 0 && remainder != 0))
		{
			nums[i] = try_add_double (numbers[i]);
		}
		else
		{
			nums[i] = numbers[i];
		}
	}
}


int calculate_sum (int *const numbers, const int size)
{
	int sum = 0;	
	int nums[size];
	int i;
	
	_double (numbers, size, size % 2, nums);
	
	for (i = 0; i < size; ++i)
	{
		sum += nums[i];
	}
	return sum;
}


boolean luhn_check (int *const numbers, const int size)
{
	int filtered_size = 0;
	int filtered[size];
	int i;
	
	for (i = 0; i < size; ++i)
	{
		if (is_number (numbers[i]))
		{
			filtered[filtered_size++] = numbers[i];
		}
	}
	
	if (filtered_size <= 16)
	{
		int sum = calculate_sum (filtered, filtered_size);

		if (sum % 10 == 0)
		{
			return TRUE;
		}
		return FALSE;
	}
	return FALSE;
}


void format_valid_luhn (int *const numbers, const int size, const int first, const int last)
{
	int i;
	
	for (i = 0; i < size; ++i)
	{
		if (i >= first && i <= last)
		{
			if (is_number (numbers[i]))
			{
				numbers[i] = 'X' - '0';
				continue;
			}
		}
	}
}


int find_first_number_index (const int *const numbers, const int size)
{
	int i;

	for (i = 0; i < size; ++i)
	{
		if (is_number (numbers[i]))
		{
			return i;
		}
	}
	return 0;
}

static sub[MAX_SIZE];


int *process (const int *const numbers, const int size, const int digits, const int first, const int last, const int acc, int *const new_numbers)
{
	int i;
	
	if (first >= size - 1)
		return new_numbers;
	
	if (digits == 14 && last == size - 1 && acc < 14)
	{
		int sub_size = 0;
		
		for (i = first; i <= last; ++i)
		{
			sub[sub_size++] = numbers[i]; 
		}
		
		boolean check = luhn_check (sub, sub_size);
		if (check)
		{
			format_valid_luhn (new_numbers, size, first, last);
			return process (numbers, size, 16, first + 1, first + 1, 0, new_numbers);
		}
		return new_numbers;
	}
	
	if (last == (size - 1) && acc < digits)
	{
		int sub_size = 0;
		
		for (i = first; i <= last; ++i)
		{
			sub[sub_size++] = numbers[i]; 
		}
		
		boolean check = luhn_check (sub, sub_size);
		if (check)
		{
			format_valid_luhn (new_numbers, size, first, last);
			return process (numbers, size, 16, first + 1, first + 1, 0, new_numbers);
		}
		return process (numbers, size, digits - 1, first, first, 0, new_numbers);		
	}
	
	if (acc == digits)
	{
		int sub_size = 0;
		
		for (i = first; i <= last; ++i)
		{
			sub[sub_size++] = numbers[i]; 
		}
		
		boolean check = luhn_check (sub, sub_size);
		if (check)
		{
			format_valid_luhn (new_numbers, size, first, last);
			return process (numbers, size, 16, first + 1, first + 1, 0, new_numbers);
		}
		
		if (digits == 14)
		{
			return process (numbers, size, 16, first + 1, first + 1, 0, new_numbers);
		}
		return process (numbers, size, digits - 1, first, first, 0, new_numbers);
	}
	
	if (digits > acc && is_number (numbers[last + 1]))
	{
		return process (numbers, size, digits, first, last + 1, acc + 1, new_numbers);
	}
	
	return process (numbers, size, digits, first, last + 1, acc, new_numbers);
}


void check (int *numbers, const int size, int *new_numbers)
{
	const int digits = get_number_of_digits (numbers, size);
	int i;

	if (digits >= 14)
	{
		int first = find_first_number_index (numbers, size);
		int i;
		
		for (i = 0; i < size; ++i)
		{
			new_numbers[i] = numbers[i];
		}
		process (numbers, size, 16, first, first, 0, new_numbers);
		return;
	}
	
	for (i = 0; i < size; ++i)
	{
		new_numbers[i] = numbers[i];
	}
}


void process_line_feed (const char *str, const int size, char *new_str)
{
	int numbers[size];
	int new_numbers[size];
	int i;
	
	for (i = 0; i < size; ++i)
	{
		numbers[i] = str[i] - '0';
	}
	
	check (numbers, size, new_numbers);
	
	for (i = 0; i < size; ++i)
	{
		new_str[i] = new_numbers[i] + '0';
	}
}


void reset_buffer (char *buffer, const int size)
{
	int i;
	
	for (i = 0; i < size; ++i)
	{
		buffer[i] = '\0';
	}
}


int main (int argc, char *argv[])
{
	char input[MAX_SIZE] = { '\0' };
	char output[MAX_SIZE] = { '\0' };
	int input_pos = 0;
	FILE *f;

	while ((input[input_pos++] = fgetc (stdin)) != EOF)
	{
		if (input[input_pos - 1] == '\n')
		{
			int i;
			
			input[input_pos] = '\0';
			process_line_feed (input, strlen (input) - 1, output);
			printf ("%s\n", output);
			
			input_pos = 0;
			reset_buffer (output, MAX_SIZE);		
		}
	}
	return 0;
}
