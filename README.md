# Problem

Make CLI app to:

* taking input file name
* simulate robot movement based on input contents
* output final location & heading

# Solution

Minimum first solution is to:

1. get input args
2. read the file
3. parse the file
4. create time series of robot movements
	* writing out all steps will make view/debugging easier later
5. output final positions from time series as output

## Optionals

Once I get this done I would like to:

1. Validate time series as legal moves before output
2. Better visualize movement of robots
3. Create a better DSL for input optimized for humans


