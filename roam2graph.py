import itertools
import json
import pathlib
import re
import sqlite3
import sys

import networkx as nx
import networkx.algorithms.link_analysis.pagerank_alg as pag
import networkx.algorithms.community as com
from networkx.readwrite import json_graph

N_COM = 7  # Desired number of communities
N_MISSING = 20  # Number of predicted missing links
MAX_NODES = 200  # Number of nodes in the final graph


def to_rellink(inp: str) -> str:
    return pathlib.Path(inp).stem


def to_kebab_case(input_string):
    # Replace spaces and underscores with hyphens
    kebab_case_string = re.sub(r'\s+|_', '-', input_string)
    # Convert to lowercase
    kebab_case_string = kebab_case_string.lower()
    return kebab_case_string


def build_graph() -> nx.DiGraph:
    """Build a graph from the org-roam database."""
    graph = nx.DiGraph()
    home = pathlib.Path.home()
    conn = sqlite3.connect(home / ".emacs.d" / "org-roam.db")

    # Query all nodes first
    nodes = conn.execute('''SELECT
                                id,
                                title,
                                CASE
                                WHEN properties LIKE '%("KIND" . "post")%' THEN 'posts'
                                WHEN properties LIKE '%("KIND" . "mycelium")%' THEN 'mycelium'
                                ELSE 'regular'
                                END AS node_type
                            FROM nodes
                            WHERE node_type != 'regular';''')
    # A double JOIN to get all nodes that are connected by a link
    links = conn.execute('''SELECT
                                n1.id,
                                n2.id
                            FROM nodes n1
                            JOIN links ON links."source" = n1.id
                            JOIN nodes n2 ON n2.id = links.dest
                            WHERE
                                links.type = '"id"'
                            UNION ALL
                            SELECT n1.id, n2.id
                            FROM nodes AS n1
                            JOIN (
                            SELECT file, MIN(pos) AS min_pos
                            FROM nodes
                            GROUP BY file
                            ) AS top_nodes
                            ON n1.file = top_nodes.file AND n1.pos = top_nodes.min_pos
                            JOIN nodes AS n2
                            ON n1.file = n2.file;''')
    # Populate the graph
    graph.add_nodes_from((n[0], {
        "id": n[0].strip("\""),
        "label": n[1].strip("\""),
        "tooltip": n[1].strip("\""),
        "title": n[1].strip("\""),
        "type": n[2]
    }) for n in nodes)
    graph.add_edges_from(n for n in links if n[0] in graph.nodes and n[1] in graph.nodes)
    conn.close()
    return graph


def compute_centrality(dot_graph: nx.DiGraph) -> None:
    """Add a `centrality` attribute to each node with its PageRank score.
    """
    simp_graph = nx.Graph(dot_graph)
    central = pag.pagerank(simp_graph)
    min_cent = min(central.values())
    central = {i: central[i] - min_cent for i in central}
    max_cent = max(central.values())
    central = {i: central[i] / max_cent for i in central}
    nx.set_node_attributes(dot_graph, central, "centrality")
    sorted_cent = sorted(dot_graph, key=lambda x: dot_graph.nodes[x]["centrality"])
    for n in sorted_cent[:-MAX_NODES]:
        dot_graph.remove_node(n)


def compute_communities(dot_graph: nx.DiGraph, n_com: int) -> None:
    """Add a `communityLabel` attribute to each node according to their
    computed community.
    """
    simp_graph = nx.Graph(dot_graph)
    communities = com.girvan_newman(simp_graph)
    labels = [tuple(sorted(c) for c in unities) for unities in
              itertools.islice(communities, n_com - 1, n_com)][0]
    label_dict = {l_key: i for i in range(len(labels)) for l_key in labels[i]}
    nx.set_node_attributes(dot_graph, label_dict, "communityLabel")


def add_missing_links(dot_graph: nx.DiGraph, n_missing: int) -> None:
    """Add some missing links to the graph by using top ranking inexisting
    links by ressource allocation index.
    """
    simp_graph = nx.Graph(dot_graph)
    preds = nx.ra_index_soundarajan_hopcroft(simp_graph, community="communityLabel")
    new = sorted(preds, key=lambda x: -x[2])[:n_missing]
    for link in new:
        sys.stderr.write(f"Predicted edge {link[0]} {link[1]}\n")
        dot_graph.add_edge(link[0], link[1], predicted=link[2])


if __name__ == "__main__":
    sys.stderr.write("Reading graph...")
    DOT_GRAPH = build_graph()
    compute_centrality(DOT_GRAPH)
    compute_communities(DOT_GRAPH, N_COM)
    add_missing_links(DOT_GRAPH, N_MISSING)
    sys.stderr.write("Done\n")
    JS_GRAPH = json_graph.node_link_data(DOT_GRAPH)
    sys.stdout.write(json.dumps(JS_GRAPH))
