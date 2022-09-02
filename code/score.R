# input "rolls" as a vector
score <- function(rolls) {
  nrolls <- length(rolls)
  ord_rl <- sort(rolls)
  df = as.data.frame(table(ord_rl))
  uniq_rolls = length(df$ord_rl)
  
  # 6 dice
  if (nrolls == 6) {
    ret_string = character()
    curr = 0
    if (uniq_rolls == 6) {
      #print('full run: 2500')
      return(2500)
    } else if ((uniq_rolls == 3) & (all(df$Freq == 2))) {
      #print('three pairs: 1500')
      return(1500)
    } else if (uniq_rolls == 1) {
      #print('six of a kind: 3000')
      return(3000)
    } else if ((uniq_rolls == 2) & (any(df$Freq) == 5)) {
      curr = 2000
      ret_string = "five of a kind"
      
      # check for 1/5
      single_id = which(df$Freq != 5)
      single_val = df$ord_rl[single_id]
      
      if (single_val == 1) {
        curr = curr + 100
        ret_string = paste0(ret_string, '; ones')
      } else if (single_val == 5) {
        curr = curr + 50
        ret_string = paste0(ret_string, '; fives')
      } 
      return(curr)
    } else if (any(df$Freq == 4)) {
      curr = 1000
      ret_string = "four of a kind"
      
      # check for 1s/5s
      ones = which(df$ord_rl == 1)
      if (length(ones) == 1) {
        curr = curr + 100 * df$Freq[ones]
        ret_string = paste0(ret_string, '; ones')
      }
      fives = which(df$ord_rl == 5)
      if (length(fives) == 1) {
        curr = curr + 50 * df$Freq[fives]
        ret_string = paste0(ret_string, '; fives')
      }
    } else if (any(df$Freq == 3)) {
      triple_id = which(df$Freq == 3)
      for (id in triple_id) {
        if (df$ord_rl[id] == 1) {
          curr = curr + 1000
          ret_string = paste0(ret_string, 'triple 1')
        } else {
          curr = curr + unique(ord_rl)[id] * 100
          ret_string = paste0(ret_string, 'triple', df$ord_rl[id])
        }
      }
      
      # check for 1s/5s
      rem = df$ord_rl[-triple_id]
      freq_rem = df$Freq[-triple_id]
      ones = which(rem == 1)
      if (length(ones) == 1) {
        curr = curr + 100 * freq_rem[ones]
        ret_string = paste0(ret_string, '; ones')
      }
      fives = which(rem == 5)
      if (length(fives) == 1) {
        curr = curr + 50 * freq_rem[fives]
        ret_string = paste0(ret_string, '; fives')
      }
    } else if (any(df$ord_rl == 1) | any(df$ord_rl == 5)) {
      # check ones
      ones_id = which(df$ord_rl == 1)
      if (length(ones_id) > 0) {
        curr = curr + df$Freq[ones_id] * 100
        ret_string = paste0(ret_string, 'ones')
      }
      
      # check fives
      fives_id = which(df$ord_rl == 5)
      if (length(fives_id) > 0) {
        curr = curr + df$Freq[fives_id] * 50
        ret_string = paste0(ret_string, 'fives')
      }
    }
    #print(paste(ret_string, ':', curr))
    return(curr)
    # five dice: 
  } else if (nrolls == 5) {
    curr = 0
    ret_string = character()
    
    # check five of a kind
    if (uniq_rolls == 1) {
      curr = 2000
      ret_string = "five of a kind"
      
      # check 4 of a kind: 
    } else if ((uniq_rolls == 2) & (any(df$Freq) == 4)) {
      curr = 1000
      ret_string = "four of a kind"
      
      # check for 1/5
      single_id = which(df$Freq != 5)
      single_val = df$ord_rl[single_id]
      
      if (single_val == 1) {
        curr = curr + 100
        ret_string = paste0(ret_string, '; ones')
      } else if (single_val == 5) {
        curr = curr + 50
        ret_string = paste0(ret_string, '; fives')
      } 
      
      # check triple: 
    } else if (any(df$Freq == 3)) {
      triple_id = which(df$Freq == 3)
      curr = 0
      ret_string = character()
      
      if (df$ord_rl[triple_id] == 1) {
        curr = curr + 1000
        ret_string = paste0(ret_string, 'triple 1')
      } else {
        curr = curr + unique(ord_rl)[triple_id] * 100
        ret_string = paste0(ret_string, 'triple', df$ord_rl[triple_id])
      }
      
      # check for 1s/5s
      rem = df$ord_rl[-triple_id]
      freq_rem = df$Freq[-triple_id]
      ones = which(rem == 1)
      if (length(ones) == 1) {
        curr = curr + 100 * freq_rem[ones]
        ret_string = paste0(ret_string, '; ones')
      }
      fives = which(rem == 5)
      if (length(fives) == 1) {
        curr = curr + 50 * freq_rem[fives]
        ret_string = paste0(ret_string, '; fives')
      }
      
      # check 1s/5s
    } else if (any(df$ord_rl == 1) | any(df$ord_rl == 5)) {
      # check ones
      ones_id = which(df$ord_rl == 1)
      if (length(ones_id) > 0) {
        curr = curr + df$Freq[ones_id] * 100
        ret_string = paste0(ret_string, 'ones')
      }
      
      # check fives
      fives_id = which(df$ord_rl == 5)
      if (length(fives_id) > 0) {
        curr = curr + df$Freq[fives_id] * 50
        ret_string = paste0(ret_string, 'fives')
      }
    }
    #print(paste(ret_string, ':', curr))
    return(curr)
    
    # 4 dice: 
  } else if (nrolls == 4) {
    curr = 0
    ret_string = character()
    
    # check four of a kind
    if (uniq_rolls == 1) {
      curr = 1000
      ret_string = "four of a kind"
      
      # check triple: 
    } else if (any(df$Freq == 3)) {
      triple_id = which(df$Freq == 3)
      curr = 0
      ret_string = character()
      
      if (df$ord_rl[triple_id] == 1) {
        curr = curr + 1000
        ret_string = paste0(ret_string, 'triple 1')
      } else {
        curr = curr + unique(ord_rl)[triple_id] * 100
        ret_string = paste0(ret_string, 'triple', df$ord_rl[triple_id])
      }
      
      # check for 1s/5s
      rem = df$ord_rl[-triple_id]
      freq_rem = df$Freq[-triple_id]
      ones = which(rem == 1)
      if (length(ones) == 1) {
        curr = curr + 100 * freq_rem[ones]
        ret_string = paste0(ret_string, '; ones')
      }
      fives = which(rem == 5)
      if (length(fives) == 1) {
        curr = curr + 50 * freq_rem[fives]
        ret_string = paste0(ret_string, '; fives')
      }
      
      # check 1s/5s: 
    } else if (any(df$ord_rl == 1) | any(df$ord_rl == 5)) {
      # check ones
      ones_id = which(df$ord_rl == 1)
      if (length(ones_id) > 0) {
        curr = curr + df$Freq[ones_id] * 100
        ret_string = paste0(ret_string, 'ones')
      }
      
      # check fives
      fives_id = which(df$ord_rl == 5)
      if (length(fives_id) > 0) {
        curr = curr + df$Freq[fives_id] * 50
        ret_string = paste0(ret_string, 'fives')
      }
    }
    #print(paste(ret_string, ':', curr))
    return(curr)
    
    # 3 dice
  } else if (nrolls == 3) {
    curr = 0
    ret_string = character()
    
    # check triple
    if (uniq_rolls == 1) {
      triple_id = which(df$Freq == 3)
      curr = 0
      ret_string = character()
      
      if (df$ord_rl[triple_id] == 1) {
        curr = curr + 1000
        ret_string = paste0(ret_string, 'triple 1')
      } else {
        curr = curr + unique(ord_rl)[triple_id] * 100
        ret_string = paste0(ret_string, 'triple', df$ord_rl[triple_id])
      }
      
      # check 1s/5s
    } else if (any(df$ord_rl == 1) | any(df$ord_rl == 5)) {
      # check ones
      ones_id = which(df$ord_rl == 1)
      if (length(ones_id) > 0) {
        curr = curr + df$Freq[ones_id] * 100
        ret_string = paste0(ret_string, 'ones')
      }
      
      # check fives
      fives_id = which(df$ord_rl == 5)
      if (length(fives_id) > 0) {
        curr = curr + df$Freq[fives_id] * 50
        ret_string = paste0(ret_string, 'fives')
      }
    }
    #print(paste(ret_string, ':', curr))
    return(curr)
    
    # 2 or 1 dice: 
  } else {
    curr = 0
    ret_string = character()
    
    # check ones
    ones_id = which(df$ord_rl == 1)
    if (length(ones_id) > 0) {
      curr = curr + df$Freq[ones_id] * 100
      ret_string = paste0(ret_string, 'ones')
    }
    
    # check fives
    fives_id = which(df$ord_rl == 5)
    if (length(fives_id) > 0) {
      curr = curr + df$Freq[fives_id] * 50
      ret_string = paste0(ret_string, 'fives')
    }
    
    #print(paste(ret_string, ':', curr))
    return(curr)
  }
}