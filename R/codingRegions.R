#' get gene coding regions (coding) grch37
#'
#' This function extracts
#' @param genes, a vctor list of hgnc gene names.
#' @keywords Glib
#' @import biomaRt
#' @export
#' @examples
#' codingRegions()


codingRegions <- function(genes) {

  genes <- "SCN5A"

  mart <- useMart(biomart="ENSEMBL_MART_ENSEMBL", host="grch37.ensembl.org", path="/biomart/martservice", dataset="hsapiens_gene_ensembl")

  P <- getBM(mart ,attributes = c("ensembl_peptide_id","ensembl_gene_id","ensembl_transcript_id", "hgnc_symbol"),
             filters = "hgnc_symbol",values = genes)
  canon <- getBM(mart , attributes = c("hsapiens_paralog_canonical_transcript_protein"), filters = c("hgnc_symbol"),values = genes)
  exons <- getBM(mart, attributes = c("ensembl_gene_id", "ensembl_exon_id","ensembl_transcript_id", "chromosome_name",
                                      "exon_chrom_start", "exon_chrom_end", "strand"), filters = "hgnc_symbol", values = genes)

  idxCan <- which(P$ensembl_peptide_id %in% canon[[1]])
  CanTrans <- P[idxCan,c(3,4)]
  geneList <- unique(P$hgnc_symbol)

  noCan <- geneList[ ! unique(P$hgnc_symbol) %in% P$hgnc_symbol[ P$ensembl_transcript_id %in% CanTrans[,1] ]]
  for(k in 1:length(noCan) ) {
    if(!identical(noCan, character(0))) CanTrans <- rbind(CanTrans,P[P$hgnc_symbol == noCan[k],][1,c(3,4)])
  }

  CanTrans <- as.character(CanTrans[order(CanTrans$hgnc_symbol),1])

  exons$hgnc_symbol <- rep(NA,length(exons$ensembl_gene_id))
  for (i in 1:nrow(exons))
    suppressWarnings(exons$hgnc_symbol[i] <- P$hgnc_symbol[ P$ensembl_gene_id ==  exons$ensembl_gene_id[i] ])

  resExons <- exons[exons$ensembl_transcript_id %in% CanTrans,c("chromosome_name","exon_chrom_start","exon_chrom_end","hgnc_symbol")]
  return(resExons)
}








