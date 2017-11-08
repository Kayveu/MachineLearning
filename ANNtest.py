import numpy as np
import random

class Network(object):

    def __init__(self, sizes):
        self.layers = len(sizes)
        self.sizes = sizes        #size is a list of integers
        self.biases = [np.random.randn(y, 1) for y in sizes[1:]]   #biases for every neuron for every layer after input layer
        #where biases is a vector
        self.thetas = [np.random.randn(y, x) for x, y in zip(sizes[:-1], sizes[1:])]     #thetas for every layer after the first
        #where thetas is a list of matrices containing all thetas

    def forwardprop(self, a):
        for b, t in zip(self.biases, self.thetas):
            a = sigmoid(np.dot(t, a) + b)         #for each layer, exact same as mnist_test.py except iterating through each layer rather than explicitly defining it
            #each time we iterate through each layer, the inputs(a) from the previous layer gets used in the new iteration
        return a

    def SGD(self, training_data, iterations, batch_size, learn_rate, test_data = None):
        if test_data:
            n_test = len(test_data)             #if test_data is not none, then wee're using the test_data set
        n = len(training_data)
        for i in xrange(iterations):
            random.shuffle(training_data)
            batches = [training_data[k:k + batch_size] for k in xrange(0, n, batch_size)] #separates training data into batches, batches = list of mini batches
                                                                                        #for each set, we take a batch and continue from that point forward
            for mini in batches:
                self.update_mini(mini, learn_rate)          #actual gradient update for each batch

            if test_data:
                print "Iteration {0}: {1} / {2}".format(i, self.evaluate(test_data), n_test)
            else:
                print "Iteration {0} complete.".format(i)

    def update_mini(self, mini, learn_rate):
        """Updates thetas and biases by gradient descent using backprop to a single batch"""
        nabla_b = [np.zeros(b.shape) for b in self.biases]
        nabla_t = [np.zeros(t.shape) for t in self.thetas]
        for x, y in mini:
            delta_b, delta_t = self.backprop(x, y)          #deltas given for gradient descent using backprop for each training example
            nabla_b = [nb + db for nb, db in zip(nabla_b, delta_b)]     #updates all biases with all bias deltas
            nabla_t = [nt + dt for nt, dt in zip(nabla_t, delta_t)]     #updates all thetas with all theta deltas
        self.thetas = [t - (learn_rate / len(mini)) * nt for t, nt in zip(self.thetas, nabla_t)] #updates all thetas
        self.biases = [b - (learn_rate / len(mini)) * nb for b, nb in zip(self.biases, nabla_b)] #updates all bias thetas

    def backprop(self, x, y):           #for every set of training examples (x, y)
        nabla_b = [np.zeros(b.shape) for b in self.biases]      #bias gradients where self.biases is a vector of [y x 1]
        nabla_t = [np.zeros(t.shape) for t in self.thetas]      #theta gradients where self.thetas is a list of matrices of thetas for every layer

        #forward prop
        activation = x
        activations = [x]               #list to store all activations, layer by layer
        zs = []                         #list to store all the z vectors layer by layer
        for b, t in zip(self.biases, self.thetas): #where thetas is a list of matrices containing all thetas and biases a vector
            z = np.dot(t, activation) + b
            zs.append(z)            #append to list for easy backprop access
            activation = sigmoid(z)
            activations.append(activation)      #append to list for easy backprop access

        #backprop
        delta = self.cost_derivative(activations[-1], y) * sigmoid_prime(zs[-1])    #error in the last layer
        nabla_b[-1] = delta                     #update bias in last layer
        nabla_t[-1] = np.dot(delta, activations[-2].transpose())        #updates weights going into output

        for l in xrange(2, self.layers):        #moving backwards from output layer to input
            z = zs[-l]
            sp = sigmoid_prime(z)
            delta = np.dot(self.thetas[-l + 1].transpose(), delta) * sp     #errors for every layer before last (iteratively backwards)
            nabla_b[-l] = delta
            nabla_t[-l] = np.dot(delta, activations[-l - 1].transpose())    #final gradient calculations for all layers
        return (nabla_b, nabla_t)               #returns matrix of gradients for every layer

    def evaluate(self, test_data):
        """Returns number of correct test inputs"""
        test_results = [(np.argmax(self.forwardprop(x)), y) for (x, y) in test_data]
        return sum(int(x == y) for (x, y) in test_results)

    def cost_derivative(self, output_activations, y):
        return (output_activations - y)

def sigmoid(z):
    return 1.0 / (1.0 + np.exp(-z))

def sigmoid_prime(z):
    return sigmoid(z) * (1 - sigmoid(z))
