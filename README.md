# FSCallTree
This command line utility extracts function definitions and call structure for Onshape FeatureScript JS-like language.
When started in the folder containing number of text files with the FS code it will create Call trees output in a new folder for every text file.
For example given the file `Array utils.txt` with the content:
```
export function groupBy(arr is array, f is function) returns map
{
    var result = {};
    for (var item in arr)
    {
        const newKey = f(item);
        if (result[newKey] is undefined)
            result[newKey] = [item];
        else
            result[newKey] = append(result[newKey], item);
    }

    return result;
}
```
the FSCallTree.exe will create a folder `CallTrees` with file `Array_utils_call_tree.txt` with content:
```
Line    Col     Scope    Name
1       8       0        |groupBy(arr is array, tolerance, f is function) returns array
5       24      1            |f
7       25      1            |keys
8       17      1            |abs
10      31      1            |append
22      21      1            |append
24      12      1            |tolerantSort
27      47      2                |function(entry)
28      11      1            |mapArray
28      20      2                |function(entry)
```
