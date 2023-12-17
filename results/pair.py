import pandas as pd
import matplotlib.pyplot as plt

def load_data(file_path):
    """Load the CSV file into a DataFrame."""
    return pd.read_csv(file_path)

def extract_individual_pairs(df):
    """Extract individual pairs from the 'pair' column and return a flattened list of pairs."""
    all_pairs = df['pair'].str.split(', ')
    return [pair for sublist in all_pairs for pair in sublist]

def count_individual_pair_frequencies(pairs):
    """Count the frequency of each individual pair."""
    return pd.Series(pairs).value_counts()

def plot_individual_pair_frequencies(pair_counts):
    """Generate a bar plot for the frequency of each individual pair."""
    plt.figure(figsize=(10, 6))
    plt.bar(pair_counts.index, pair_counts.values)
    plt.title("Frequency of Each Individual Pair")
    plt.xlabel("Pair")
    plt.ylabel("Frequency")
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.show()
    
def transform_pair_format(pair_str):
    """Transform the pair string to a comparable format."""
    return ', '.join(['{' + pair.replace('-', ',') + '}' for pair in pair_str.split(', ')])

def count_exact_matches(df):
    """Count the number of exact matches between expected and detected pairs of signals."""
    df['transformed_pair'] = df['pair'].apply(transform_pair_format)
    return (df['listS'] == df['transformed_pair']).sum()

def calculate_average_misclassification_rate(df):
    """Calculate and return the average of the EPairClass column."""
    return df['EPairClass'].mean()


def main():
    file_path = input("Enter the path of the CSV file: ")
    df = load_data(file_path)

    # Extracting and counting individual pair frequencies
    individual_pairs = extract_individual_pairs(df)
    pair_counts = count_individual_pair_frequencies(individual_pairs)
    # print(pair_counts)
    # Counting exact matches
    
    exact_matches = count_exact_matches(df)
    print(f"Number of exact matches: {exact_matches}")

    # Calculating the average misclassification rate
    average_misclassification_rate = 100*calculate_average_misclassification_rate(df)
    print(f"The average misclassification rate is: {average_misclassification_rate:.3f}")

    # Generating the bar plot
    plot_individual_pair_frequencies(pair_counts)

if __name__ == "__main__":
    main()
