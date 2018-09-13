
main = do content <- getContents
          let ls = lines content
              in print $ head ls
