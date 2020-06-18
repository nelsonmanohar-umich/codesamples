DISPLAY_THRESHOLD = 0.3
N = 250
M = 10

t = as.matrix(read.csv( 'minhash.csv', header=FALSE, sep=',', nrows=N))
r = read.csv( 'datafile.csv', header=FALSE, nrows=N)
idx = which( t < DISPLAY_THRESHOLD )
t[idx] = 0

colors = heat.colors(M)
colors = terrain.colors(M)
rsc = ifelse( r$V2 == 1, "blue", "black")

# RowSideColor tip from SebastianRashcha website

tuple_corrs = t[1:N,1:N]
graphics.off()
png( 'plot_clt_heatmaps_input_samples-1.png', 1200, 800)
    heatmap( tuple_corrs[1:(N*1),1:(N*1)], scale="none", RowSideColor=rsc, Rowv=NA, Colv=NA, revC=TRUE, col=colors, main="Heat Map for Time Ordered Input Vectors")
dev.off()

png( 'plot_clt_heatmaps_input_samples-2.png', 1200, 800)
    heatmap( tuple_corrs[1:(N*1),1:(N*1)], scale="none", RowSideColor=rsc, col=colors, main="Heat map for Input Vectors (Time Order Not Preserved)")
dev.off()

png( 'plot_clt_heatmaps_input_samples-3.png', 1200, 800)
    plot( hclust( dist( tuple_corrs[1:(N*1),1:(N*1)])), main="Dendrogram for Clusters of Input Vectors")
dev.off()
