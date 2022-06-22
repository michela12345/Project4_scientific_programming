#' Title Show the alignment of a mapped read
#'
#' @param ref The genome
#' @param bam My BAM/SAM files
#' @param cigar The alignment that I want
#'
#' @return The position of the alignment
#' @author Michela Francesconi\cr Politecnico di Milano\cr Maintainer: Michela
#' Francesconi\cr E-Mail: <michelafrancesconi8@gmail.com>
#' @export align
#' @importFrom Biostrings DNAString
#' @importFrom Biostrings DNA_ALPHABET
#'
#' @examples
#' align(('AAGTCGTAGAATACGTAAGTCGTAGAATACGT'),('AACACGTAG..TAGCTAACACGTAG..TAGCT'),('2S5M2D2M'))
#' @export
align <- function(ref, bam, cigar){
  bam <- DNAString(bam)
  ref <- DNAString(ref)
  indice_ex <- 1

  for (j in 1:length(ref)){

    successo <- TRUE
    indice_in <- j
    indice_posizione <- j


    for (i in 1:(nchar(cigar)/2)){
      numero <- substr(cigar, (i*2)-1, (i*2)-1)
      lettere <- substr(cigar, i*2, i*2)
      indice_in <- indice_in + as.numeric(numero)

      if (indice_in > length(ref)){
        successo <- FALSE
        break

      }

      while (indice_posizione < indice_in){
        if (lettere == "S") {
          if (ref[indice_posizione] == bam[indice_posizione]){
            break
          }
          else{
            indice_posizione <- indice_posizione +1
          }
        }
        else if (lettere == "M" || lettere == '=' || lettere == 'X'){
          if (ref[indice_posizione] == bam[indice_posizione]){
            indice_posizione <- indice_posizione +1

          }
          else{
            break
          }


        }

        else if (lettere == 'I'){
          if (DNAString(DNA_ALPHABET[18])[1] == ref[indice_posizione]){
            indice_posizione <- indice_posizione+1
          }
          else{
            break
          }
        }

        else if (lettere == "D" || lettere == 'N'){
          if (DNAString(DNA_ALPHABET[18])[1] == bam[indice_posizione]){
            indice_posizione <- indice_posizione +1

          }
          else{
            break
          }

        }
      }
      if (indice_posizione != indice_in){
        successo <- FALSE
        break
      }

    }
    if (successo){
      print(j:indice_posizione)
    }
  }

}



