---
title: "Value Driven Development"
date: 2024-01-06 00:00:00
featured_image: "/images/articles/m/value-driven-pipelines.png"
excerpt: Centering devops pipelines around value delivered to market
---

### Introduction

The problem with most development teams is that the value to be provided is often compromised by structural issues within the organization or technical problems in the technology stack. Poor engineering decisions made early in the process could have led to the accumulation of technical debt that ballooned before your arrival, or you may have contributed to it yourself due to a lack of experience and succumbing to the temptations of what seemed like rapid progress.

There is a much better way to ship your code. That is to agree on a definition of monetizable value among your organization and determine the best ways to measure it, both before and after it's delivered, and center the entire engineering solution and team structure around delivering this value.

### Defining Monetizable Value

Monetizable value, for the sake of this post, will be defined as follows:

> The threshold of utility that, once provided and then withdrawn, would produce a noticeable and negative effect in the life of the recipient.

I have chosen this definition as my guiding principle for determining how much utility the first N number of features must provide to a user before a monetary exchange is desired. I believe it sets a higher threshold than what is strictly necessary to secure a monetary exchange, making it safer and increasing customer retention.

### Testing Value

As with TDD (Test-Driven Development), a process where programmers build their unit tests first and then provide the necessary code to meet the program specifications, value can be tested before we even begin to write a single line of code.

As a feature is conceived, we must do what we can to test the concept itself. There may be data available from competitors who are creating similar products and services that you can use. In that case, you can build upon their data, and you only need to create tests for your own unique aspects of the value they have already monetized.

The methods of validation you choose will necessarily be specific to your service or product, but some common ones are:

| Method                                      | Description                                                                                     |
| ------------------------------------------- | ----------------------------------------------------------------------------------------------- |
| Customer Interviews and Feedback            | Direct interaction with potential customers for insights on their needs and preferences.        |
| Minimum Viable Product (MVP)                | Develop a basic version of your product to test the market with minimal features.               |
| Market Research                             | Analyze the market size, competition, trends, and similar products or services.                 |
| Surveys and Questionnaires                  | Distribute surveys to a targeted audience to gauge interest and collect data.                   |
| Landing Page Testing                        | Create a landing page to describe your product and monitor the response.                        |
| Pilot Programs or Beta Testing              | Launch your product to a limited audience before full-scale deployment for real-world feedback. |
| Crowdfunding Campaigns                      | Use platforms like Kickstarter to validate your idea through public response.                   |
| Pre-Sales and Early Bird Offers             | Offer your product for pre-sale or with special offers to gauge customer interest.              |
| Social Media and Online Communities         | Utilize social media and forums for feedback and community building.                            |
| Competitive Analysis                        | Evaluate competitors and identify market gaps for a unique value proposition.                   |
| Analyzing Data Trends and Consumer Behavior | Use tools like Google Trends to understand consumer interests.                                  |
| A/B Testing                                 | Implement A/B testing on various features for online services to see what works best.           |

### The First Monetizable Feature Set (MFS)

The first feature is typically very important in a design process. If we think of it in terms of the [Pareto Principle](https://en.wikipedia.org/wiki/Pareto_principle), then 20% of our features are going to yield 80% of the value delivered to our customers. It's entirely possible to reach this 80/20 ratio with the initial monetizable feature set. This is a process of experimentation and discovery. Value needs to be validated and shipped in rapid iterations until this threshold is discovered. Create a pipeline of all the features you expect to include in version 1 of your product, prioritize them, and start shipping.

> Credit to [Ram](https://www.linkedin.com/in/ramcsingh/) for inspiring me on this one :D

An example first MFS from my current project is:

- User authenication
- Voice transcription
- Text to speech
- ChatGPT integration

Given this example, it's easy to see in my case the 80/20 is already realized in this first MFS that will be shipped. It is expected to only take 2 weeks for just myself to ship this to Google Play! and the Apple App Store starting from scratch.

### The Team

The DevOps process needs to ensure that when a feature enhancement is defined, and the sub-tasks are all assigned, the team is capable of integrating all their work products seamlessly. In other words, from the perspective of a feature, despite any unrelated tasks carried out by team members, the tasks directly related to completing this feature must be finished in the necessary time and order so that the work can be assembled in the DevOps pipeline and delivered to the customer.

This concept can be quite novel for many organizations, especially in larger ones, where the backend and frontend often belong to different teams. This can lead to interdependent waiting states during development, slowing down the delivery of value to the market.

The ideal situation, as exemplified by teams like those who built [WhatsApp](https://thechipletter.substack.com/i/131589058/whatsapp), is to keep the engineering team small and prioritize competence over size. Ideally, the entire team should be able to contribute to every aspect of the engineering work required to build and deliver the software. This means that every team member should have a comprehensive understanding of DevOps, programming in the target languages, cloud engineering, multiplatform development, and UI design.

With a team like this, the strategy of prioritizing the delivery of value and shipping features quickly becomes straightforward, especially if the target programming language is the same for both the backend and frontend, and your project can compile to all platform targets from a single codebase.

### Summary

Throughout my time as a software developer, I've encountered numerous teams that ship poor-quality code and fail to prioritize value. They frequently invest far more time than necessary coding in isolation before bringing their product to the market. Focus all your efforts on rapidly achieving the value that can be monetized to ensure sustainability as soon as possible. Organize your engineering solutions and human resources in a way that optimizes the delivery of value to the customer and meets the target.
