cypher-mode
===========

Emacs major mode for editing cypher scripts ([Neo4j](http://neo4j.org/))


Sample cypher statement

    START me=node:node_auto_index(name="Me") 
    FOREACH (i in range(1,10) : 
    CREATE (friend {name: "Friend "+i}), (me)-[:FRIEND]->(friend));

