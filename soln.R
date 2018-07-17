tree_labelled = T
# while there are still strains unlabelled
while (!all(nodes%in%strains)) {
  # find children of selected node
  children = sapply(strsplit(edges[edges[,1]==node, 2], "[-]"), function(x) x[1])
  # if not all child nodes are strains, pick first one and go down deeper
  # otherwise, find a unknown strain not in tree yet with date earlier than children and is closest to the inferred current strain
  if (any(!children%in%strains)) {
    node = children[!children%in%strains]
    node = node[1]
  } else {
    # get indices of candidate parent strains
    min_date = min(as.Date(as.character(meta[children, "date"])))
    min_date_ = seq(min_date, length=2, by="-6 months")[2]
    dm_colind = which(dates_unkn < min_date & 
                        dates_unkn > min_date_ &
                        !names(dates_unkn)%in%nodes & 
                        !strains_unkn%in%nodes)
    # if no strains satisfy our contraints, end loop
    if (length(dm_colind)==0) {
      print("incomplete! need data on older strains")
      tree_labelled = F
      break()
    }
    # get parent strain that is closest to inferred ancestral strain
    parent = names(dm_colind[which.min(dm_anc[node, dm_colind])])
    edges[edges==node] = nodes[nodes==node] = node_strain = parent
    # nodes_in = setdiff(nodes_in, node)
    node = edges[edges[,2]==node_strain, 1]
  }
}
save(edges, file=paste(result_dir,"/edges.Rdata", sep=""))
save(nodes, file=paste(result_dir,"/nodes.Rdata", sep=""))

