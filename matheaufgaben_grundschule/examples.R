# Test the generating function
generate_exercise_list(Nexercise = 10, difficulty = 2, type = 1)
generate_exercise_list(Nexercise = 10, difficulty = 2, type = 2)

# Test plotting
ex1 <- generate_exercise_list(Nexercise = 20, difficulty = 3, type = 2)
plot(ex1, base_size = 10)

pdf(paste0('matheaufgaben_grundschule/example_output/example_', Sys.time(), '.pdf'), paper = 'a4')
plot(ex1)
dev.off()

