split_data_simple <- function(data, n) {
  # Çalışmada  n = 1.5, 2, 3, 4, 5, 6, 7 olarak seçilmiştir
  # sırasıyla, 3, 5, 10, 15, 20, 25, 30 batch sayısı elde edilir.
  subset_size <- floor(nrow(data) / n)
  
  train_size <- floor(subset_size * 0.8)
  test_size <- subset_size - train_size
  
  train_set <- data[1:train_size, ]
  test_set <- data[(train_size + 1):(train_size + test_size), ]
  
  list(total = dim(data)[1] ,train_set = dim(train_set)[1], test_set = dim(test_set)[1])
}
