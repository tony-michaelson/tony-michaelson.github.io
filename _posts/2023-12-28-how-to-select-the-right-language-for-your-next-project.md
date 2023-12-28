---
title: "How to Select the Right Language for your Next Project"
date: 2023-12-28 00:00:00
featured_image: "/images/articles/program-any-language-b.png"
excerpt: Choosing the Right Programming Language for Your Project
---

<script src="https://cdnjs.cloudflare.com/ajax/libs/Chart.js/4.4.1/chart.umd.min.js" integrity="sha512-CQBWl4fJHWbryGE+Pc7UAxWMUMNMWzWxF4SQo9CgkJIN1kx6djDQZjh3Y8SZ1d+6I+1zze6Z7kHXO7q3UyZAWw==" crossorigin="anonymous" referrerpolicy="no-referrer"></script>

<script src="/js/concurrency-tests-data.json"></script>

Continuing from [Crossing The Language Barrier](/blog/crossing-the-language-barrier), we're going to take a look at some good strategies we can implement when considering programming languages for a project. Using this [language selection table](/blog/crossing-the-language-barrier#selecting-the-right-language), let's see what this might look like in practice.

| ---------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Nature of the Project | Mobile application for Android and IOS users with possible future requirement for web users. Cloud architecture for back-end with a mix of relational and non-relational data storage. Backend API integrations with various 3rd party services. Backend is nothing complex and just mixing/connecting AI and cloud services for specific use cases. Team wants to use container hosting. Strongest requirement is for a POC to be stood up in 2 weeks. |
| Performance Needs | Necessity for scaling in response to surges in user request volume. Many user requests could be heavy on text processing operations. There are no strict SLAs for response times for any requests. |
| Language Features | Concurrency, strong typing, and basic FP features |
| Community and Ecosystem | A mainstream ecosystem like maven, npm, etc. |
| Long-term Maintenance | Fully automated and documented devops pipelines, standard gitflow, gitlab project, automated unit/integration testing, regular dependency update intervals, agile, near-zero tech-debt, and hand off to another team for LTS |
| Personal Resonance and Team Skills | Ideally same language for the front end and backend for speed and to enable any team member to work on any part of the system |

After investigation, various candidates have been identified as the top choices given the capabilities for mobile development and using the same language for the backend. Of these options Dart and Kotlin would be completely new to me.

- Dart (w/Flutter)
- TypeScript (w/React Native or Ionic)
- Kotlin Multiplatform (with Ktor for backend)

### Comparing Options:

Let's start by comparing the runtime environments.

- dart/exe 3.0.6
- node 20.4.0
- bun 0.6.14
- kotlin/jvm 17.0.2

Of these options I have only worked (more than superficially) with the JVM.

#### a). Performance:

Performance metrics from [programming-language-benchmarks.vercel.app](https://programming-language-benchmarks.vercel.app/) provide some good benchmarks for test cases that are similar to the work our backend will need to perform for various use cases.

| Test Case Name         | Runtime Environment | Time (ms) |
| ---------------------- | ------------------- | --------- |
| Binary Trees           | Kotlin/JVM 17.0.2   | 639ms     |
| Binary Trees           | Dart/exe 3.0.6      | 932ms     |
| Binary Trees           | Bun 0.6.14          | 1144ms    |
| Binary Trees           | Node 20.4.0         | 3424ms    |
| HTTP Server Input 3000 | Bun 0.6.14          | 139ms     |
| HTTP Server Input 3000 | Dart/exe 3.0.6      | 1597ms    |
| HTTP Server Input 3000 | Node 20.4.0         | 3174ms    |
| HTTP Server Input 3000 | Kotlin/JVM 17.0.2   | timeout   |
| HTTP Server Input 500  | Bun 0.6.14          | 99ms      |
| HTTP Server Input 500  | Node 20.4.0         | 400ms     |
| HTTP Server Input 500  | Dart/exe 3.0.6      | 1222ms    |
| HTTP Server Input 500  | Kotlin/JVM 17.0.2   | 2423ms    |
| JSON Serde             | Bun 0.6.14          | 128ms     |
| JSON Serde             | Node 20.4.0         | 152ms     |
| JSON Serde             | Dart/exe 3.0.6      | 266ms     |
| JSON Serde             | Kotlin/JVM 17.0.2   | 1007ms    |
| Regex Redux 250000_in  | Dart/exe 3.0.6      | 758ms     |
| Regex Redux 250000_in  | Kotlin/JVM 17.0.2   | 1078ms    |
| Regex Redux 250000_in  | Node 20.4.0         | n/a       |
| Regex Redux 250000_in  | Bun 0.6.14          | n/a       |
| nbody                  | Dart/exe 3.0.6      | 521ms     |
| nbody                  | Kotlin/JVM 17.0.2   | 532ms     |
| nbody                  | Node 20.4.0         | 611ms     |
| nbody                  | Bun 0.6.14          | 1018ms    |

If the results from the tests are averaged they would be:

| Runtime Environment | Average Time (ms) |
| ------------------- | ----------------- |
| Bun 0.6.14          | 505.6             |
| Dart/exe 3.0.6      | 882               |
| Kotlin/JVM 17.0.2   | 1135              |
| Node 20.4.0         | 1552.2            |

Overall, given the results of these performance tests, it looks like the top two candidates are Bun and Dart. The 5000ms+ timeout for Kotlin/JVM isn't ideal for the higher iteration input test on the http server and will be assessed deeper if Kotlin surfaces as a favorite candidate.

#### b). Concurrency Models:

Concurrency refers to a computer system's ability to manage multiple tasks or operations simultaneously.

The primary differences in concurrency models between Kotlin, Dart (with Flutter), and Node.js lie in their underlying approaches to managing concurrent operations. Kotlin leverages coroutines, offering a lightweight and efficient means of handling concurrency through suspending functions and structured concurrency. Dart relies on isolates, which isolate separate code instances with their own memory, making it well-suited for parallelism and asynchronous tasks, especially in the context of mobile development with Flutter. Node.js adopts an event-driven, single-threaded architecture, providing excellent performance for I/O-bound tasks and allowing for optional worker threads to address CPU-bound workloads.

Each model has its advantages, with Kotlin excelling in structured concurrency, Dart focusing on isolates, and Node.js offering an event-driven and optional multi-threaded approach, catering to a wide range of use cases.

| Aspect                 | Kotlin                                    | Dart                                   | Node.js                                          |
| ---------------------- | ----------------------------------------- | -------------------------------------- | ------------------------------------------------ |
| Concurrency Mechanism  | Coroutine-based concurrency model.        | Concurrent execution via isolates.     | Concurrent execution with workers.               |
| Execution Environment  | Uses Kotlin coroutines within the JVM.    | Isolated memory heap for each isolate. | Isolated context for each worker.                |
| Memory Sharing         | Supports shared mutable state with care.  | No shared memory.                      | Optional shared memory with `SharedArrayBuffer`. |
| Communication          | Coroutine-based asynchronous programming. | Message passing between isolates.      | Message passing between workers.                 |
| Structured Concurrency | Yes                                       | No, but leverages isolates.            | No, but supports async/await and callbacks.      |

#### c). Concurrency Testing Setup:

<img src="/images/articles/assets/lb-concurrency-test.png" width="200" alt="Nginx Concurrency Setup">

It's wise to conduct your own testing, even if it's purely anecdotal, as it provides a deeper understanding of the technologies within the scope of your project.

For this article, I've developed a simple yet highly informative testing scaffold that allows us to integrate various runtime environments and measure their performance with different batches of concurrent request types.

> You can find the repository [here](https://github.com/tony-michaelson/concurrency-testing-framework/tree/main).

The purpose of this testing framework is to establish local testing infrastructure that closely resembles our expected production setup. It allows us to assess how different programming languages and HTTP frameworks perform in concurrency testing scenarios that resemble real-world tasks for this project.

For this, we require at least:

- A Network Load Balancer
- A Pool of Backend HTTP Servers
- Strict CPU and memory resource allocations

I've also included an additional server to simulate streaming and regular HTTP calls to external API services. The setup looks like this:

```
                +-------------+
                |             |
                |   NGINX     |
                |   Load      |
                |   Balancer  |
                |             | ------------------------------------------------
                +-------------+                              |                  |
                  /       |       \                          |                  |
                 /        |        \                         |                  |
                /         |         \                        |                  |
               /          |          \                       |                  |
              /           |           \                      |                  |
             /            |            \                     |                  |
+------+-------+  +------+-------+  +------+-------+  +------+-------+  +-------+--------+
|              |  |              |  |              |  |              |  |                |
|   server1    |  |   server2    |  |   server3    |  |   server4    |  |    streaming   |
|              |  |              |  |              |  |              |  |                |
+--------------+  +--------------+  +--------------+  +--------------+  +----------------+
```

The endpoints provided by the HTTP server pools will remain the same regardless of the programming language we use to handle these requests. I have endpoint tests in place to ensure that the requests appear identical for each solution I will be testing. When you deploy the system, you will notice that these tests are automatically executed to ensure that the server pool uniformly handles our requests.

```
$ ./up.sh
[+] Building 0.0s (0/0)                                                                                               docker:desktop-linux
WARN[0000] Found orphan containers ([nodejs-debugger-1]) for this project. If you removed or renamed this service in your compose file, you can run this command with the --remove-orphans flag to clean it up.
[+] Running 7/0
 ✔ Container nodejs-server4-1           Created                                                                                       0.0s
 ✔ Container nodejs-streaming_server-1  Created                                                                                       0.0s
 ✔ Container nodejs-server2-1           Created                                                                                       0.0s
 ✔ Container nodejs-server3-1           Created                                                                                       0.0s
 ✔ Container nodejs-server1-1           Created                                                                                       0.0s
 ✔ Container nodejs-nginx-1             Created                                                                                       0.0s
 ✔ Container nodejs-test-1              Created                                                                                       0.0s
Attaching to nodejs-nginx-1, nodejs-server1-1, nodejs-server2-1, nodejs-server3-1, nodejs-server4-1, nodejs-streaming_server-1, nodejs-test-1
nodejs-server3-1           | Server is running on port 3003
nodejs-server4-1           | Server is running on port 3004
nodejs-server2-1           | Server is running on port 3002
nodejs-server1-1           | Server is running on port 3001
nodejs-streaming_server-1  | Server is running on port 3030
nodejs-streaming_server-1  | Test passed: Stream route
nodejs-streaming_server-1  | Test passed: API call route
...
nodejs-test-1              |   HTTP Streaming Server Tests
nodejs-test-1              |     GET /stream
nodejs-test-1              |       ✔ should return a status code of 200 and stream data (1726ms)
nodejs-test-1              |     GET /api-call
nodejs-test-1              |       ✔ should return a status code of 200 (5063ms)
nodejs-test-1              |
nodejs-test-1              |   HTTP Server Tests
nodejs-test-1              |     POST /input
nodejs-test-1              |       ✔ should return 'Processed Input: 4 elements' with a status code of 200
nodejs-test-1              |     GET /hello
nodejs-test-1              |       ✔ should return 'Hello!' a status code of 200 (48ms)
nodejs-test-1              |     GET /string-concat
nodejs-test-1              |       ✔ should return expected data with a status code of 200 (51ms)
nodejs-test-1              |     GET /cpu
nodejs-test-1              |       ✔ should return a list of prime numbers with a status code of 200 (952ms)
nodejs-test-1              |     GET /consume
nodejs-test-1              |       ✔ should return some data with a status code of 200 (996ms)
nodejs-test-1              |     GET /api-call
nodejs-test-1              |       ✔ should return a valid JSON response with a status code of 200 (5052ms)
nodejs-test-1              |
nodejs-test-1              |
nodejs-test-1              |   8 passing (14s)
nodejs-test-1              |
nodejs-test-1 exited with code 0
```

#### Backend Server:

| PATH           | Description                                                                     |
| -------------- | ------------------------------------------------------------------------------- |
| /input         | Deserialize JSON and walk the object tree                                       |
| /hello         | Respond with "Hello!"                                                           |
| /string-concat | Perform CPU-intensive string operation and stream response                      |
| /cpu           | Find $NUMBER_OF_PRIMES using coro-prime-sieve then respond with the list        |
| /consume       | Consume a stream from the streaming_server and respond with data once completed |
| /api-call      | Make a GET HTTP request to the streaming_server and return the response         |
| Default (404)  | Handle unknown routes with a "Not Found" response                               |

#### Streaming Server:

| PATH          | Description                                       |
| ------------- | ------------------------------------------------- |
| /stream       | Stream data with a word stream                    |
| /api-call     | Simulate an API call with a delayed response      |
| Default (404) | Handle unknown routes with a "Not Found" response |

> `/api-call` and `/consume` will call `/api-call` and `/stream` on the streaming server.

This simulation mirrors real-world scenarios that are likely to occur in a cloud environment.

The primary objective is to create task work that closely resembles our actual use cases. I aim to observe how each solution performs under conditions that closely resemble our product's deployment in a production environment.

These tasks also serve to test how different solutions handle a mix of simple requests executed concurrently with CPU-intensive ones. Additionally, tasks like `/consume` will concurrently consume `/stream`. This diverse set of operations includes both long and short-running I/O operations, time-consuming CPU operations, and memory-intensive tasks. The variety of request types will undoubtedly highlight the strengths of different runtime environments.

#### d). Concurrency Testing Results:

These charts depict the number of requests on the X-axis and the corresponding response times in milliseconds on the Y-axis. For instance, a value of 10 on the X-axis indicates that, for each request type, 10 requests were created simultaneously.

> _Reminder: The 'api call' represents an HTTP request to a backend service with an arbitrary delay of 1000ms before it responds._

To understand the resource allocations for these tests, please refer to the [docker-compose.yml](https://github.com/tony-michaelson/concurrency-testing-framework/blob/main/kotlin/docker-compose.yml) file. It's important to note that these tests are minimal and purely anecdotal, conducted on a local system.

<a href="https://michaelson.blog/pages/concurrency-test-results.html" target="_blank">Test Results Page</a>

#### Analysis

While these are anecdotal local system tests, they have provided us with valuable insights to aid in our decision-making process.

1). Bun consistently outperforms Node.js when using identical server code and system resources, up until the higher request per second (RPS) test at 44 concurrent requests, where it encounters performance issues.

2). Node.js outperforms Dart/exe on intensive CPU tasks, large JSON posts, and object walking tasks.

3). Dart/exe performs better than Bun, Node.js, and Kotlin in simple request types when CPU-bound tasks are mixed in. The Dart runtime excels at prioritizing these task types, whereas other runtimes tend to prioritize CPU-bound tasks at the expense of simpler ones.

4). Kotlin/jre performs exceptionally well and completes a variety of tasks with a better overall average response time than the other runtime environments. I/O tasks on the main thread for Node.js and Bun are handled more efficiently and have lower response times than Kotlin when CPU-bound tasks are ongoing. Surprisingly, overall response times are lower in tests that don't involve expensive CPU-bound tasks.

#### e). Development Experience

##### Kotlin

To set up a Kotlin multiplatform project in IntelliJ, the website directs us to install the plugin and create a new project using Android Studio. However, this process doesn't automatically set up a target for the backend. Fortunately, with some investigation, I discovered this [useful project starter tool](https://kmp.jetbrains.com/) from JetBrains.

Initially, the project didn't build successfully, and it required some experimentation with different Java versions until I understood the necessary configuration. Additionally, I had to accept the Android licenses, which necessitated using Java 8, and manually copy the license files into the project. None of these steps were immediately obvious to us, and the error messages were somewhat misleading.

Once I got the project up and running, it became easy to understand the layout and relationships between the sub-projects. I also found a Gradle task for running the backend and deploying the project to Android or iOS for testing the UIs. The turnaround time for making a simple UI change and applying it to the app in the Android simulator on my machine was approximately 2.5 seconds after the initial setup.

It's worth noting that the UI API is the same as [Jetpack Compose](https://developer.android.com/jetpack/compose/documentation), and the Compose Multiplatform project implements this API for other platforms.

##### Flutter

Setting up a Flutter project was straightforward, and everything worked out of the box. While there isn't an option for including a backend project, it's easy enough to add one manually. The [Flutter UI](https://docs.flutter.dev/ui) framework compiles the UI for all platform targets, and the hot reload feature works very well for all the platforms I tested. Flutter's ability to maintain the application state between reloads makes UI development remarkably smooth. Intuitively, it feels like a significant time-saving in the UI development process.

##### Summary

| Aspect                                 | Kotlin Multiplatform                       | Dart with Flutter                    |
| -------------------------------------- | ------------------------------------------ | ------------------------------------ |
| **Platform Targets**                   | Android, iOS, Desktop, Web (alpha), Server | Android, iOS, Web, Desktop, Embedded |
| **UI Framework**                       | Compose Multiplatform, UIKit SwiftUI       | Flutter UI framework                 |
| **Community and Ecosystem**            | Growing community                          | Large, active community              |
| **IDE Support**                        | IntelliJ or Android Studio                 | VS Code, Android Studio, IntelliJ    |
| **Hot Reload**                         | No                                         | Yes                                  |
| **Performance and Native Integration** | Deep integration with native code          | Flutter rendering engine             |
| **Maturity**                           | Evolving, gaining adoption                 | Mature, well-established             |

### Conclusion

If your backend workload requirements involve complex CPU tasks, Kotlin emerges as the most compelling choice for developing a single-language, full-stack, multiplatform application. This preference is rooted in the concurrency grammar seamlessly integrated into Kotlin/ktor, simplifying code comprehension and enhancing backend performance. While it's worth noting that transitioning to Kotlin might demand additional time if you're unfamiliar with other JVM languages, and meticulous attention to thread management settings and JVM memory allocation is essential for backend development.

Conversely, when your backend primarily deals with straightforward task types, Dart stands out as an excellent option, especially when considering its advantages in UI development through Flutter. Dart's approach to isolates and message passing is intuitive, and asynchronous code remains highly readable. As of December 2023, it's important to mention that Dart lacks tail-call optimization, which may require functional programmers to adapt their strategies in specific cases. Nevertheless, Dart's language design fosters a low-friction, rapid learning experience. For TypeScript developers, transitioning to Dart should pose minimal challenges, and they can expect to be productive from day one.

It's challenging to definitively recommend one language over the other, as the choice depends on your specific project requirements. Both Kotlin and Dart exhibit unique strengths that make them compelling options. I trust that this discussion has provided valuable insights to aid you in your decision-making process. Happy coding!
