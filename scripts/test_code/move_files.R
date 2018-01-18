# File file error

regions.todo <- read.csv("../cyipt/input-data/RegionsToDo.csv", stringsAsFactors = F)
regions.todo <- regions.todo[!is.na(regions.todo$do),]
regions.todo <- regions.todo$region[regions.todo$do == "y"]

for(b in 1:length(regions.todo)){
  if(file.exists(paste0("../cyipt-bigdata/osm-prep/",regions.todo[b],"/schemes-simplified.Rds"))){
    file.copy(from = paste0("../cyipt-bigdata/osm-prep/",regions.todo[b],"/schemes-simplified.Rds"),
              to = paste0("../cyipt-bigdata/osm-recc/",regions.todo[b],"/schemes-simplified.Rds"),
              overwrite = TRUE)
    file.remove(paste0("../cyipt-bigdata/osm-prep/",regions.todo[b],"/schemes-simplified.Rds"))
  }else{
    message(paste0("File Missing ",regions.todo[b]))
  }
}

schemes <- readRDS(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/schemes-simplified.Rds"))
names(schemes)


