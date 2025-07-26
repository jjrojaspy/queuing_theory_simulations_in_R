# Queueing Theory Simulation:
#
# etba: exponential time between arrivals.
# etos: exponential time of service.
# cnoc: constant number of customers
#
# Type of Queue: infinite.
# Arrival process: time ~ exponential(TIME_BETWEEN_ARRIVALS_RATE).
# Queue discipline: FIFO.
# Service process: time ~ exponential(TIME_OF_SERVICE_RATE).
# System capacity:
#   Number of customers allowed: infinite.
#   System availability time: 8 hours.
# Number of servers: 1.
# Simul. has a queue? No.
minute <- next_arrival_time <- service_time <- .0

times_between_arrivals <- service_times <- number_of_costumers_arriving <- vector()

queue_counter <- 0

total_hours <- 8.0
total_minutes <- total_hours * 60
next_arrival_flag <- TRUE
while (minute < total_minutes || queue_counter > 0) {
  if (minute <= total_minutes && next_arrival_flag) {
    time_between_arrivals <- rexp(n = 1, rate = 10)

    next_arrival_time <- minute + time_between_arrivals
    times_between_arrivals <- append(times_between_arrivals, time_between_arrivals)

    next_arrival_flag <- FALSE
  }

  if (abs(minute - next_arrival_time) < 1.0) {
    number_of_costumers_arriving <- append(number_of_costumers_arriving, sample(x = c(2, 4, 8), size = 1, prob = c(.4, .5, .1)))
    queue_counter <- queue_counter + number_of_costumers_arriving[length(number_of_costumers_arriving)]

    next_arrival_flag <- TRUE
  }

  if (queue_counter > 0) {
    if (service_time <= 0) {
      service_time <- rexp(n = 1, rate = 10)

      service_times <- append(service_times, service_time)

      queue_counter <- queue_counter - 1
    }
  }

  if (service_time > .0) {
    service_time <- service_time - 1
  }

  minute <- minute + 1
}

length(times_between_arrivals)
length(number_of_costumers_arriving)
length(service_times)

mean(times_between_arrivals)
mean(number_of_costumers_arriving)
mean(service_times)
