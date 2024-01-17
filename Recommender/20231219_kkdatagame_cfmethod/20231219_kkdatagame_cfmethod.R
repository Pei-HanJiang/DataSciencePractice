library(dplyr)
library(arrow)
library(tidyr)
library(parallel)

#multicore for windows
source("mclapply_hack.R")

#jaccard similarity 
jaccard <- function(x, y) {
  length(intersect(x, y))/length(union(x, y))
}

vjaccard = Vectorize(jaccard, "y")

is.hit <- function(x, y) {
  length(intersect(x , y)) > 0
}

trn = read_parquet("label_train_source.parquet")
tgt = read_parquet("label_train_target.parquet")
tids = tgt$session_id %>% unique

#long table to wide table
dw = pivot_wider(trn[,c(1,2,6)], names_from = listening_order, values_from = song_id)
ids = dw$session_id
dw = as.matrix(dw[,-1])

# decide the target session
tid = tids[1]
# get the first 20 songs in the target session
xx = dw[ids == tid,]

# caculate the jaccard similarity between the target session to all the sessions
ssim = apply(dw, 1, jaccard, x=xx)

# get the top 0.01% similar session as candidate
cids = ids[ssim %>%
             order(decreasing = T) %>%
             head(ceiling(0.0001*length(ssim)))]
cids = setdiff(cids, tid)

# get the songs in the candidate sessions as the candidate songs
csongs = dw[ids %in% cids,] %>% as.vector %>% unique

# get the songs in the target session
tsongs = trn$song_id[trn$session_id == tid]

# get the sessions listening to the songs in the target session
ids.same.song = c(trn$session_id[trn$song_id %in% tsongs], 
                  tgt$session_id[tgt$song_id %in% tsongs]) %>%
  unique %>%
  setdiff(tid)

# for each candidate songs, get the sessions listening to the song
sid.csongs = mclapply(1:length(csongs), function(i) {
  print(i)
  s =csongs[i]
  c(trn$session_id[trn$song_id == s],
    tgt$session_id[tgt$song_id == s]) %>%
    unique}, 
  mc.cores = 7)

# calculate the similarity between the candidate songs and the target session
sngsim = vjaccard(ids.same.song, sid.csongs)

# get the recommended songs (top 30)
rsongs = csongs[order(sngsim, decreasing = T) %>% head(30)]

# remove the songs listened in the target session from the recommendation list
rsongs = setdiff(rsongs, tsongs)

# evaluation
(rsongs %in% tgt$song_id[tgt$session_id == tid]) %>% which
