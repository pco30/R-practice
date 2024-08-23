# Import necessary libraries and the Fashion MNIST dataset
library(keras)
library(tidyr)
library(ggplot2)

# Load the Fashion MNIST dataset
fashion <- dataset_fashion_mnist()

# Split the data into training and testing sets
# Training size is 60,000 images and test size is 10,000 images
c(train_images, train_labels) %<-% fashion$train
c(test_images, test_labels) %<-% fashion$test

# Define class labels corresponding to the Fashion MNIST dataset
class_names <- c('T-shirt/top',
                 'Trouser',
                 'Pullover',
                 'Dress',
                 'Coat',
                 'Sandal', 
                 'Shirt',
                 'Sneaker',
                 'Bag',
                 'Ankle boot')

# Data exploration: Check the dimensions of the datasets
dim(train_images)  # Should return 60000 x 28 x 28
dim(train_labels)  # Should return 60000
dim(test_images)   # Should return 10000 x 28 x 28
dim(test_labels)   # Should return 10000

# Data preprocessing: Normalize the pixel values from [0, 255] to [0, 1]
# This helps with faster convergence during training
train_images <- train_images / 255
test_images <- test_images / 255

# Visualize the first training image using ggplot2
image_1 <- as.data.frame(train_images[1, , ])
colnames(image_1) <- seq_len(ncol(image_1))
image_1$y <- seq_len(nrow(image_1))
image_1 <- gather(image_1, "x", "value", -y)
image_1$x <- as.integer(image_1$x)

ggplot(image_1, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black", na.value = NA) +
  scale_y_reverse() +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  theme(aspect.ratio = 1) +
  xlab("") +
  ylab("")

# Plot multiple training images with their corresponding labels
par(mfcol = c(5, 5)) # Set up a 5x5 plotting grid
par(mar = c(0, 0, 1.5, 0), xaxs = 'i', yaxs = 'i')
for (i in 1:25) { 
  img <- train_images[i, , ]
  img <- t(apply(img, 2, rev)) # Rotate image for correct orientation
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste(class_names[train_labels[i] + 1]))
}

# Build the neural network model using Keras
model <- keras_model_sequential()
model %>%
  layer_flatten(input_shape = c(28, 28)) %>%  # Flatten the 28x28 images into vectors
  layer_dense(units = 128, activation = 'relu') %>%  # Dense hidden layer with 128 neurons and ReLU activation
  layer_dense(units = 10, activation = 'softmax')  # Output layer with 10 neurons (one for each class) and softmax activation

# Display the model's architecture
summary(model)

# Compile the model with optimizer, loss function, and metrics
model %>% compile(
  optimizer = 'adam',  # Adam optimizer
  loss = 'sparse_categorical_crossentropy',  # Loss function for multi-class classification
  metrics = c('accuracy')  # Track accuracy during training
)

# Train the model with the training data
model %>% fit(
  train_images, train_labels, epochs = 20, validation_split = 0.2
)

# Evaluate the model on the test data
score <- model %>% evaluate(test_images, test_labels)
cat('Test loss:', score$loss, '\n')
cat('Test accuracy:', score$acc, '\n')

# Make predictions on the test data
predictions <- model %>% predict(test_images)

# Print the prediction vector for the first test image
predictions[1,]
# Identify the class with the highest probability for the first test image
which.max(predictions[1,])
# Display the true label of the first test image
test_labels[1]

# Plot the first 25 test images with their predicted and true labels
par(mfcol = c(5, 5))
par(mar = c(0, 0, 1.5, 0), xaxs = 'i', yaxs = 'i')
for (i in 1:25) { 
  img <- test_images[i, , ]
  img <- t(apply(img, 2, rev))  # Rotate image for correct orientation
  predicted_label <- which.max(predictions[i, ]) - 1  # Get the predicted class
  true_label <- test_labels[i]  # Get the true class
  color <- if (predicted_label == true_label) 'black' else 'red'  # Color the title based on correctness
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste0(class_names[predicted_label + 1], " (", class_names[true_label + 1], ")"),
        col.main = color)
}
