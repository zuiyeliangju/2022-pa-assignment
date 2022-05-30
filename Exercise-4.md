Exercise 4 - Shan
================

## Illustration of bus seat relationship network

We’ll use `tidygraph` package to create and plot the seat network from
the slides. We’ll be relying on these tutorials:

-   <http://users.dimi.uniud.it/~massimo.franceschet/ns/syllabus/make/tidygraph/tidygraph.html>
-   <https://www.data-imaginist.com/2017/introducing-tidygraph/>
-   <https://www.data-imaginist.com/2018/tidygraph-1-1-a-tidy-hope/>

First, we define the network by manually entering all the nodes and the
connections between them. We’ll define `nodes` table that has two
columns: `id` and `name`. We will then define the connections among them
as an edgelist, where each element in column `from` corresponds to a
friend on one end of the relationship and each element in column `to`
corresponds to the person on the other end of this friendship tie.

``` r
# define nodes
node_names <- tibble(
  id   = c(1,2,3,4,5,6,7,8,9,10),
  name = c("Coworker 1","Coworker 2","Coworker 3","Coworker4","Coworker 5","Coworker 6", "A","B","C","D")
)
node_names
```

    ## # A tibble: 10 × 2
    ##       id name      
    ##    <dbl> <chr>     
    ##  1     1 Coworker 1
    ##  2     2 Coworker 2
    ##  3     3 Coworker 3
    ##  4     4 Coworker4 
    ##  5     5 Coworker 5
    ##  6     6 Coworker 6
    ##  7     7 A         
    ##  8     8 B         
    ##  9     9 C         
    ## 10    10 D

``` r
# define connections (have to correspond to ties 1-2, 2-3, 2-4, 3-4)
# for each element in `from` there is a corresponding element in `to`
edge_list <- tibble(
  from = c(1,2,7,7,8,8,8,10,10,10,6,5,3,3),
  to   = c(2,7,9,8,9,6,10,6,5,3,5,3,4,9)
)
edge_list
```

    ## # A tibble: 14 × 2
    ##     from    to
    ##    <dbl> <dbl>
    ##  1     1     2
    ##  2     2     7
    ##  3     7     9
    ##  4     7     8
    ##  5     8     9
    ##  6     8     6
    ##  7     8    10
    ##  8    10     6
    ##  9    10     5
    ## 10    10     3
    ## 11     6     5
    ## 12     5     3
    ## 13     3     4
    ## 14     3     9

We can now combine these tables into a “graph” object that holds all of
our network data.

``` r
# combine this information into a network graph object
friendship_graph <- tbl_graph(nodes = node_names, edges = edge_list, directed = FALSE)
friendship_graph
```

    ## # A tbl_graph: 10 nodes and 14 edges
    ## #
    ## # An undirected simple graph with 1 component
    ## #
    ## # Node Data: 10 × 2 (active)
    ##      id name      
    ##   <dbl> <chr>     
    ## 1     1 Coworker 1
    ## 2     2 Coworker 2
    ## 3     3 Coworker 3
    ## 4     4 Coworker4 
    ## 5     5 Coworker 5
    ## 6     6 Coworker 6
    ## # … with 4 more rows
    ## #
    ## # Edge Data: 14 × 2
    ##    from    to
    ##   <int> <int>
    ## 1     1     2
    ## 2     2     7
    ## 3     7     9
    ## # … with 11 more rows

We can now plot this network using `ggraph` package.

``` r
friendship_graph %>% 
    ggraph(layout = 'kk') + 
    geom_edge_link() + 
    geom_node_point(size = 8, colour = 'gray') +
    geom_node_text(aes(label = name), colour = 'steelblue', vjust = 0.4) + 
    ggtitle('Friendship network') + 
    theme_graph()
```

![](Exercise-4-quick-start_files/figure-gfm/plot-graph-1.png)<!-- -->

We can use many of the functions in package `tidy_graph` to calculate
things we want to know about this network. For example, we may want to
know the centrality of each node in the network.

``` r
friendship_graph <- friendship_graph %>% 
  activate(nodes) %>% # we need to state we'll be adding to nodes, not edges
  mutate(d_centrality = centrality_degree()) %>%  # adding measure of degree centrality
  mutate(b_centrality = centrality_betweenness()) # adding betweenness centrality
```

    ## Warning in betweenness(graph = graph, v = V(graph), directed = directed, :
    ## 'nobigint' is deprecated since igraph 1.3 and will be removed in igraph 1.4

``` r
friendship_graph
```

    ## # A tbl_graph: 10 nodes and 14 edges
    ## #
    ## # An undirected simple graph with 1 component
    ## #
    ## # Node Data: 10 × 4 (active)
    ##      id name       d_centrality b_centrality
    ##   <dbl> <chr>             <dbl>        <dbl>
    ## 1     1 Coworker 1            1          0  
    ## 2     2 Coworker 2            2          8  
    ## 3     3 Coworker 3            4         10.5
    ## 4     4 Coworker4             1          0  
    ## 5     5 Coworker 5            3          1  
    ## 6     6 Coworker 6            3          1.5
    ## # … with 4 more rows
    ## #
    ## # Edge Data: 14 × 2
    ##    from    to
    ##   <int> <int>
    ## 1     1     2
    ## 2     2     7
    ## 3     7     9
    ## # … with 11 more rows

Now let’s plot this with degree centrality determining the size of the
nodes and betweenness determining its color.

``` r
friendship_graph %>% 
  ggraph(layout = 'kk') + 
  geom_edge_link() + 
  geom_node_point(aes(size = d_centrality, colour = b_centrality)) + 
  scale_color_continuous(guide = 'legend') +
  geom_node_text(aes(label = name), colour = 'red', vjust = 1.6) + 
  ggtitle('Friendship network') + 
  theme_graph()
```

![](Exercise-4-quick-start_files/figure-gfm/plot-centrality-1.png)<!-- -->

## Discuss possible consequences of your choice of a seat. When would this choice be beneficial? When would it be not so beneficial?

My first impression of the best seat is A, since it has the highest
centrality when we consider both types of centrality (10 of b_centrality
and 3 of d_centrality), which means A is playing an important role on
connecting coworker 2&1 and the rest of the group, and alos has a good
number of total connections.

However, by reviewing the background information again, I changed my
decision into seat B, since my role is a summer internship, my goal is
to make connections as many as possible instead of playing an important
connection role in a team. B has a fair b_centrality value (5), and the
d_centrality value is 4, which is one of the highest.

D has the same d_centrality value as B, but has 0 as the b_centrality
value. C has the same b_centrality level, but less d_centralti value.

So I think seat B is the most beneficial choice if I am just a summer
intern.

B will not be the best choice if I am a long-term team player, who will
be benefit from a connector poistion, like seat A.
