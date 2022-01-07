# cluster_annotation_operator

##### Description

The `cluster annotation operator` use the MeM score value and a cell_type-marker table to associate cell population to cluster.

##### Usage

Input projection|.
---|---
`y-axis`        | numeric, MeM score value 
`row`           | type, list of channels
`column`        | type, Cluster id  
`labels`        | type, documentID of the file containing the cell_type-marker table.

Input parameters|.
---|---
`Positive Threshold`        | numeric value defining the threshold between the express markers (+) and the over-express markers (++)

Output relations|.
---|---
`output_var`        | output relation

##### Details



##### See Also

[MEM_operator](https://github.com/tercen/MEM_operator)
[flowsom_operator](https://github.com/tercen/flowsom_operator)
