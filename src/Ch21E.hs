module Ch21E where


type TI = []

main = do
  let trigger :: TI (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)
