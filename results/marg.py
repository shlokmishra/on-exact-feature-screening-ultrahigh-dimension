import pandas as pd
import matplotlib.pyplot as plt

def load_data(file_path):
    """Load the CSV file into a DataFrame."""
    return pd.read_csv(file_path)

def count_exact_matches(df):
    """Count the number of exact matches between 'listS' and 'marg'."""
    df['listS_split'] = df['listS'].str.split(', ')
    df['marg_split'] = df['marg'].str.split(', ')
    return sum(df['listS_split'] == df['marg_split'])

def plot_detected_signal_frequencies(df):
    """Generate a bar plot for the frequency of each detected signal."""
    detected_signals = sum(df['marg_split'], [])  # Flatten the list of lists
    signal_counts = pd.Series(detected_signals).value_counts()
    
    plt.figure(figsize=(10, 6))
    plt.bar(signal_counts.index, signal_counts.values)
    plt.title("Detected Signal Frequencies")
    plt.xlabel("Signal")
    plt.ylabel("Frequency")
    plt.show()

def calculate_average_misclassification_rate(df):
    """Calculate and return the average of the 'EmarClass' column."""
    return df['EmarClass'].mean()

def main():
    file_path = input("Enter the path of the CSV file: ")
    df = load_data(file_path)

    # Counting exact matches
    exact_matches = count_exact_matches(df)
    print(f"Number of exact matches: {exact_matches}")

    # Calculating the average misclassification rate
    average_misclassification_rate = 100*calculate_average_misclassification_rate(df)
    print(f"The average misclassification rate is: {average_misclassification_rate:.3f}%")

    # Generating the bar plot
    plot_detected_signal_frequencies(df)

if __name__ == "__main__":
    main()
