# FSCallTree
This command line utility extracts function definitions and call structure for Onshape FeatureScript JS-like language.
When started in the folder containing number of text files with the FS code it will create Call trees output in a new folder for every text file.
For example given the file `Array utils.txt` with the content:
```
export function groupBy(arr is array, tolerance, f is function) returns array
{
    var result = {};
    for (var item in arr)
    {
        const newKey = f(item);
        var appended = false;
        for (var key in keys(result))
            if (abs(key - newKey) <= tolerance)
            {
                result[key] = append(result[key], item);
                appended = true;
                break;
            }

        if (!appended)
            result[newKey] = [item];
    }

    //sorting resulting array
    var resultArr = [];
    for (var key, value in result)
        resultArr = append(resultArr, [key, value]);

    return tolerantSort(resultArr, tolerance, function(entry)
        {
            return entry[0];
        })
        ->mapArray(function(entry)
        {
            return entry[1];
        });
}
```
the FSCallTree.exe will create a folder `CallTrees` with file `Array_utils_call_tree.txt` with content:
```
Line    Col     Scope    Name
1       8       0        |groupBy(arr is array, tolerance, f is function) returns array
6       24      1            |f
8       25      1            |keys
9       17      1            |abs
11      31      1            |append
23      21      1            |append
25      12      1            |tolerantSort
28      47      2                |function(entry)
29      11      1            |mapArray
29      20      2                |function(entry)
```
