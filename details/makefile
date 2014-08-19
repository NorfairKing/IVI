all:
	make dep_graph.png
	make dep_graph_empty.png

%.png: %.dot
	bash makedot.sh $<
