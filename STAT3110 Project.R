
# Create dataframe for UVA buildings
buildings <- data.frame(
  Building_Name = c(
    "Brooks Hall",
    "Bryan Hall",
    "Clark Hall",
    "Clemons Library",
    "Cobb Hall",
    "Cocke Hall",
    "Dawson's Row #1",
    "Dawson's Row #2",
    "Dawson's Row #3",
    "Dawson's Row #4",
    "Garrett Hall",
    "Gibson Hall",
    "Halsey Hall",
    "Hotel F / Levering Hall",
    "Kerchof Hall",
    "Minor Hall",
    "Monroe Hall",
    "Nau Hall",
    "New Cabell Hall",
    "Newcomb Hall",
    "Old Cabell Hall",
    "Peabody Hall",
    "Randall Hall",
    "Rouss-Robertson Hall",
    "Rotunda",
    "Shannon Library",
    "Shumway Hall",
    "Small Special Collections Library",
    "South Lawn Commons Building",
    "University Bookstore",
    "Varsity Hall",
    "Warner Hall",
    "Wilson Hall"
  ),
  Year_Built = c(
    1877, 1995, 1932, 1981, 1917, 1896,
    1931, 1890, 1885, 1859, 1908,
    2009, 1952, 1826, 1955, 1908,
    1929, 2009, 1952, 1958, 1896,
    1914, 1897, 1896, 1826, 1938,
    2025, 2004, 2009, 1994, 1858,
    1942, 1969
  )
)

# View the dataframe
View(buildings)

# Check the structure
str(buildings)

# Summary of construction years
summary(buildings$Year_Built)

buildings$Stratum <- cut(
  buildings$Year_Built,
  breaks = c(-Inf, 1920, 1992, Inf),
  labels = c("Before 1920", "1920-1991", "1992 or later"),
  right = FALSE
)

# Calculate population size for each stratum
stratum_sizes <- table(buildings$Stratum)

# Display population sizes
print(stratum_sizes)

# Verify total population
sum(stratum_sizes)

n <- 10
N <- sum(stratum_sizes)

# Calculate exact proportional allocation
proportional_n <- n * stratum_sizes / N

# Start with integer allocations
allocation <- floor(proportional_n)

# Distribute remaining sample slots to strata
# with the largest fractional remainders
remaining <- n - sum(allocation)

if (remaining > 0) {
  order_remainder <- order(
    proportional_n - allocation,
    decreasing = TRUE
  )
  allocation[order_remainder[1:remaining]] <-
    allocation[order_remainder[1:remaining]] + 1
}

# Verify minimum sample size per stratum
stopifnot(all(allocation >= 2))
stopifnot(sum(allocation) == 10)

# Create summary table
allocation_table <- data.frame(
  Stratum = names(stratum_sizes),
  Population_Size = as.integer(stratum_sizes),
  Proportional_n = round(as.numeric(proportional_n), 2),
  Final_Sample_Size = as.integer(allocation)
)

print(allocation_table)

set.seed(123)

# Assign unique IDs to all 33 buildings
buildings$Building_ID <- 1:nrow(buildings)

# Stratum 1: Before 1920 (select 5 buildings)
stratum1 <- subset(buildings, Stratum == "Before 1920")
sample1 <- stratum1[sample(nrow(stratum1), 5, replace = FALSE), ]

# Stratum 2: 1920-1989 (select 3 buildings)
stratum2 <- subset(buildings, Stratum == "1920-1991")
sample2 <- stratum2[sample(nrow(stratum2), 3, replace = FALSE), ]

# Stratum 3: 1990 or later (select 2 buildings)
stratum3 <- subset(buildings, Stratum == "1992 or later")
sample3 <- stratum3[sample(nrow(stratum3), 2, replace = FALSE), ]

# Combine all sampled buildings
final_sample <- rbind(sample1, sample2, sample3)

# Display selected buildings
print(final_sample[, c("Building_ID", "Building_Name",
                       "Year_Built", "Stratum")])

# Verify sample sizes in each stratum
table(final_sample$Stratum)

# Verify total sample size = 10
nrow(final_sample)



