# Required Libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(stringdist)
library(pdftools)
library(rlang)


# ---- UI ----
ui <- dashboardPage(
  dashboardHeader(title = "CDP Chatbot 🚀"),
  
  dashboardSidebar(
    fileInput("upload_doc", "📄 Upload PDF/TXT Document", accept = c(".pdf", ".txt")),
    sidebarMenu(
      menuItem("💬 Chat", tabName = "chat", icon = icon("comments")),
      menuItem("📊 Comparison", tabName = "compare", icon = icon("balance-scale")),
      menuItem("ℹ️ About", tabName = "about", icon = icon("info-circle")),
      menuItem("❓ Help", tabName = "help", icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    # Background Styling
    tags$head(
      tags$style(HTML("
        body {
          background-image: url('background.jpg');
          background-size: cover;
          background-attachment: fixed;
        }
        .box {
          background-color: rgba(255, 255, 255, 0.9);
          border-radius: 15px;
          box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.1);
        }
        .chat-box {
          max-height: 400px;
          overflow-y: auto;
          border: 1px solid #ccc;
          padding: 15px;
          background-color: #f9f9f9;
        }
      "))
    ),
    
    tabItems(
      # Chat Tab
      tabItem(tabName = "chat",
              fluidRow(
                box(
                  title = tagList(icon("robot"), "CDP Chatbot"),
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  selectInput(
                    "platform", 
                    "Select CDP Platform:",
                    choices = c("Segment", "mParticle", "Lytics", "Zeotap")
                  ),
                  
                  selectInput(
                    "predefined_question",
                    "Choose a Question:",
                    choices = NULL  # Dynamic based on platform
                  ),
                  
                  textInput("user_input", "Or type your question:", ""),
                  
                  actionBttn("send", "Send", style = "material-flat", color = "primary"),
                  br(),
                  
                  tags$div(
                    class = "chat-box",
                    uiOutput("chat_log")
                  ),
                  
                  downloadButton("download_chat", "📥 Download Chat History")
                )
              )
      ),
      
      # Comparison Tab
      tabItem(tabName = "compare",
              fluidRow(
                box(
                  title = "📊 CDP Platform Comparison",
                  width = 12,
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  checkboxGroupInput(
                    "cdp_select", 
                    "Select Platforms to Compare:",
                    choices = c("Segment", "mParticle", "Lytics", "Zeotap"),
                    selected = c("Segment", "mParticle")
                  ),
                  
                  actionBttn("compare_btn", "Compare", style = "material-flat", color = "success"),
                  
                  br(),
                  tableOutput("comparison_table")
                )
              )
      ),
      
      # About Tab
      tabItem(tabName = "about",
              h3("About CDP Chatbot"),
              p("This chatbot provides support for Segment, mParticle, Lytics, and Zeotap."),
              p("Features include dynamic responses, document upload, and cross-platform comparison.")
      ),
      
      # Help Tab
      tabItem(tabName = "help",
              h3("How to Use"),
              tags$ul(
                tags$li("Select a platform from the dropdown."),
                tags$li("Choose a question or type your own."),
                tags$li("Upload documents for dynamic search."),
                tags$li("Click 'Send' to receive an answer."),
                tags$li("Compare platforms in the Comparison tab.")
              )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  chat_log <- reactiveVal("")
  uploaded_content <- reactiveVal("")
  
  # Knowledge Base
  knowledge_base <- list(
    
    "Segment" = list(
      "How to set up tracking in Segment?" = "Implement the Segment SDK in your application and add tracking calls for the desired events. Verify data flow using the debugger.",
      "How to add a new data source in Segment?" = "Go to the 'Connections' tab, click on 'Catalog', select the data source, and follow the setup steps.",
      "How to configure an integration in Segment?" = "Go to the 'Connections' tab, choose 'Catalog', select the integration, and follow the provided setup instructions.",
      "How to implement event tracking in Segment?" = "Define the events to track, add appropriate code in your application, and use the Segment debugger to ensure data is captured.",
      "How to debug data flow in Segment?" = "Use the Segment debugger in the workspace to monitor incoming and outgoing events.",
      "How to manage user identities in Segment?" = "Utilize the `identify` call to assign unique user IDs and merge identities across multiple sources.",
      "How to use Segment Personas for audience building?" = "Access the Personas feature, define traits, and build dynamic audiences based on behavioral data.",
      "How to create custom traits in Segment?" = "Go to the Personas section, create new computed traits using the UI, and apply them to audience segments.",
      "How to connect Segment with Google Analytics?" = "Install the Google Analytics integration from the 'Catalog', configure the settings, and map the events.",
      "How to export data from Segment to AWS S3?" = "Set up the AWS S3 destination from the 'Catalog', provide your bucket details, and enable the integration.",
      "How to use Segment Functions for custom workflows?" = "Use the Functions feature in the workspace to create custom integrations or transformations.",
      "How to monitor delivery metrics in Segment?" = "Navigate to the 'Metrics' tab in your workspace to analyze the data delivery performance.",
      "How to automate workflows in Segment?" = "Leverage Segment Functions and Personas to automate data pipelines and audience updates.",
      "How to handle data governance in Segment?" = "Enable the Data Governance feature to track schema violations and enforce data quality.",
      "How to anonymize user data in Segment?" = "Enable data masking in your workspace settings to anonymize sensitive fields.",
      "How to use the Segment API?" = "Access the API documentation, authenticate using your credentials, and interact with available endpoints.",
      "How to troubleshoot failed integrations in Segment?" = "Check the debugger for error messages and ensure the destination settings are correct.",
      "How to enable consent management in Segment?" = "Use Segment Consent Manager to handle user consent and apply GDPR/CCPA compliance.",
      "How to set up identity resolution in Segment?" = "Enable identity resolution in Personas to automatically merge user profiles.",
      "How to perform A/B testing using Segment?" = "Integrate with A/B testing tools like Optimizely and route experiment data via Segment.",
      "How to integrate Segment with CRM platforms?" = "Install the CRM integration from the 'Catalog', configure the credentials, and map data fields.",
      "How to implement server-side tracking in Segment?" = "Use Segment's server-side SDKs to send events directly from your backend.",
      "How to schedule data exports in Segment?" = "Use the Scheduled Functions or destinations to automate data exports.",
      "How to track custom events in Segment?" = "Define custom events in your SDK implementation and send them to the desired destinations.",
      "How to secure sensitive data in Segment?" = "Enable data masking and use encryption settings in your workspace."
    ),
    "mParticle" = list(
      "How to configure an integration in mParticle?" = "Navigate to 'Connections', choose a partner integration, and follow the setup instructions in the mParticle documentation.",
      "How to set up event tracking in mParticle?" = "Integrate the mParticle SDK into your app, then use the `logEvent` method to track specific user actions.",
      "How to enable real-time data syncing in mParticle?" = "Ensure that your data streams and configured integrations support real-time data delivery in the mParticle UI.",
      "How to manage user identities in mParticle?" = "Use mParticle's Identity API to set user IDs and manage identity resolution across devices.",
      "How to implement data filtering in mParticle?" = "Use the data filters in the 'Connections' tab to control which events or attributes are sent to each integration.",
      "How to enable consent management in mParticle?" = "Implement the Consent Management module in the SDK to track user consent and enforce GDPR/CCPA compliance.",
      "How to debug data issues in mParticle?" = "Use the 'Live Stream' feature in the mParticle UI to monitor real-time event data flow.",
      "How to integrate mParticle with Google Ads?" = "Set up Google Ads as a destination, configure the required account credentials, and map events appropriately.",
      "How to create audience segments in mParticle?" = "Go to the 'Audiences' tab, create a new audience, and define membership criteria based on user attributes.",
      "How to configure partner integrations in mParticle?" = "Select the integration partner in the 'Catalog', provide API credentials, and save the configuration.",
      "How to set up e-commerce tracking in mParticle?" = "Use the e-commerce specific tracking methods in the SDK, such as `logPurchase`, to capture transactions.",
      "How to handle GDPR compliance in mParticle?" = "Enable Consent Management, configure your privacy settings, and use the Identity API to enforce GDPR rules.",
      "How to anonymize data in mParticle?" = "Use the Data Anonymization feature in your account settings to mask sensitive user information.",
      "How to connect mParticle with BigQuery?" = "Add BigQuery as a data warehouse destination, provide the necessary credentials, and configure the schema.",
      "How to automate workflows in mParticle?" = "Use the mParticle Automation feature to trigger workflows based on specific events or conditions.",
      "How to manage data plans in mParticle?" = "Define and enforce data plans in the 'Data Master' section to standardize event naming and attributes.",
      "How to track in-app purchases in mParticle?" = "Use the SDK's `logPurchase` or `logTransaction` method to track purchases with relevant metadata.",
      "How to configure cross-device tracking in mParticle?" = "Enable the Identity Sync feature and ensure consistent user identifiers across devices.",
      "How to handle offline data uploads in mParticle?" = "Use the SDK's offline mode to cache events and sync them when a network connection is restored.",
      "How to monitor data pipeline health in mParticle?" = "Use the 'Health' dashboard to track event delivery and monitor integration statuses.",
      "How to implement server-side tracking in mParticle?" = "Use the mParticle Events API to send data directly from your server-side application.",
      "How to set up push notifications in mParticle?" = "Integrate with a push notification provider and configure user notification preferences in the SDK.",
      "How to perform A/B testing with mParticle?" = "Integrate with A/B testing tools and use mParticle to route experiment data to your destinations.",
      "How to integrate mParticle with email marketing tools?" = "Select an email marketing tool in the 'Catalog', provide API credentials, and map user attributes.",
      "How to manage data privacy settings in mParticle?" = "Configure privacy settings in the Data Master and enforce compliance rules for data sharing."
    ),
    "Lytics" = list(
      "How to create a segment in Lytics?" = "Go to the 'Audience' tab, click 'New Segment', and define segment criteria using behavioral data.",
      "How to build a predictive model in Lytics?" = "Use the 'Insights' tab to train models based on user behavior and attributes.",
      "How to personalize marketing campaigns in Lytics?" = "Leverage user segments and predictions to dynamically tailor campaign content.",
      "How to enable identity resolution in Lytics?" = "Activate identity resolution in the 'Settings' tab to merge profiles based on user identifiers.",
      "How to manage audience lists in Lytics?" = "Use the 'Audience Manager' to create, edit, and export audience lists.",
      "How to integrate Lytics with Salesforce?" = "Install the Salesforce integration from the 'Integrations' tab, provide API credentials, and map fields.",
      "How to automate campaigns in Lytics?" = "Use the Campaign Builder to create automated workflows based on user triggers.",
      "How to configure data imports in Lytics?" = "Use the 'Data Sources' tab to import data via API, flat files, or connectors.",
      "How to connect Lytics with Google Ads?" = "Enable the Google Ads integration, configure account settings, and sync user lists.",
      "How to enable real-time audience updates in Lytics?" = "Ensure data streams are active and integration settings allow for real-time updates.",
      "How to analyze user engagement in Lytics?" = "Use the 'Engagement' dashboard to visualize user behavior and trends.",
      "How to implement event tracking in Lytics?" = "Add the Lytics JavaScript tag to your site and define event tracking rules.",
      "How to activate data integrations in Lytics?" = "Go to 'Integrations', choose the tool, and configure the required settings.",
      "How to build behavior-based segments in Lytics?" = "Define rules based on user behavior using the segment builder.",
      "How to configure data filters in Lytics?" = "Set up filters in the 'Data' tab to refine imported datasets.",
      "How to manage user consent in Lytics?" = "Implement consent management modules to handle opt-in and opt-out preferences.",
      "How to troubleshoot data syncing in Lytics?" = "Check the sync logs in the 'Integration Health' section for error details.",
      "How to export audience data from Lytics?" = "Use the 'Export' button in the Audience Manager to download user data.",
      "How to set up email marketing campaigns in Lytics?" = "Integrate with an email provider and use segments to target specific users.",
      "How to analyze customer lifetime value in Lytics?" = "Leverage predictive analytics and segmentation to calculate CLV.",
      "How to integrate Lytics with social media platforms?" = "Enable social media integrations and sync audience lists for campaigns.",
      "How to manage data privacy in Lytics?" = "Enable privacy compliance features and configure rules for user data retention.",
      "How to implement identity graphs in Lytics?" = "Enable identity graphs to link multiple user identifiers into a unified profile.",
      "How to perform cross-channel marketing with Lytics?" = "Use unified profiles and segmentation to create campaigns across multiple channels.",
      "How to secure customer data in Lytics?" = "Enable encryption and apply access controls to ensure data security."
    ),
    "Zeotap" = list(
    "How to enable identity resolution in Zeotap?" = "Use the Identity API or dashboard to connect and merge identifiers across sources.",
    "How to secure customer data in Zeotap?" = "Enable encryption and enforce role-based access controls to secure sensitive data.",
    "How to integrate Zeotap with ad platforms?" = "Activate the ad platform integration from the catalog and map user data appropriately.",
    "How to activate data segments in Zeotap?" = "Use the 'Segments' module to create and activate data segments for your campaigns.",
    "How to configure consent management in Zeotap?" = "Implement the Consent Management feature to handle GDPR and CCPA compliance.",
    "How to access Zeotap's API?" = "Refer to the API documentation, generate your API key in the dashboard, and interact with available endpoints.",
    "How to manage data onboarding in Zeotap?" = "Use the Data Onboarding tool to upload and format datasets for integration into the platform.",
    "How to monitor data quality in Zeotap?" = "Enable data quality checks in the dashboard to validate incoming datasets for accuracy and completeness.",
    "How to handle GDPR compliance in Zeotap?" = "Enable the Consent Management feature and configure data privacy rules in your account settings.",
    "How to create audience segments in Zeotap?" = "Go to the 'Segments' module, define your criteria, and save the audience for activation.",
    "How to anonymize user data in Zeotap?" = "Use the data masking feature in the settings to anonymize sensitive attributes before processing.",
    "How to connect Zeotap with CDPs?" = "Set up the integration in the catalog, provide API credentials, and test the connection to your CDP.",
    "How to implement data enrichment in Zeotap?" = "Use the Data Enrichment module to append third-party attributes to your existing datasets.",
    "How to track data usage in Zeotap?" = "Monitor data consumption and segment performance in the 'Analytics' section of the dashboard.",
    "How to manage data partnerships in Zeotap?" = "Use the 'Partnerships' module to manage agreements and track data usage across partners.",
    "How to integrate Zeotap with CRM tools?" = "Select your CRM in the catalog, configure API keys, and map fields to sync data.",
    "How to perform lookalike modeling in Zeotap?" = "Leverage Zeotap's AI tools to build lookalike audiences based on existing user profiles.",
    "How to use Zeotap for fraud detection?" = "Enable fraud detection features and integrate with analytics tools to monitor suspicious activities.",
    "How to automate workflows in Zeotap?" = "Use the workflow automation feature to trigger actions based on predefined conditions.",
    "How to analyze audience insights in Zeotap?" = "Access the 'Analytics' dashboard to view detailed insights on audience behavior and performance.",
    "How to handle cross-device tracking in Zeotap?" = "Enable cross-device tracking in the Identity Resolution module to unify user profiles across devices.",
    "How to implement data suppression in Zeotap?" = "Use the suppression list feature to exclude certain users from specific campaigns.",
    "How to set up custom dashboards in Zeotap?" = "Use the dashboard builder to create custom views and metrics tailored to your needs.",
    "How to manage data privacy in Zeotap?" = "Enable privacy compliance features in the settings and enforce encryption for all data transfers.",
    "How to troubleshoot data syncing in Zeotap?" = "Check the sync logs and error messages in the dashboard to identify and resolve issues."
)

  )
  
  # Dynamic Update of Predefined Questions Based on Selected Platform
  observeEvent(input$platform, {
    updateSelectInput(session, "predefined_question", choices = knowledge_base[[input$platform]])
  })
  
  # Comparison Data
  comparison_data <- data.frame(
    Feature = c("Real-time Data Sync", "Identity Resolution", "Data Privacy Compliance", "Integration Ease"),
    Segment = c("Yes", "Basic", "GDPR/CCPA", "High"),
    mParticle = c("Yes", "Advanced", "GDPR/CCPA", "Moderate"),
    Lytics = c("Partial", "Moderate", "GDPR", "Moderate"),
    Zeotap = c("Yes", "Advanced", "GDPR/CCPA", "High")
  )
  
  # Comparison Functionality
  observeEvent(input$compare_btn, {
    selected_cdps <- input$cdp_select
    output$comparison_table <- renderTable({
      comparison_data %>% select(Feature, all_of(selected_cdps))
    })
  })
  
  # Chat Functionality
  observeEvent(input$send, {
    # Select user input or predefined question
    user_question <- ifelse(input$predefined_question != "", input$predefined_question, input$user_input)
    platform <- input$platform
    
    # Retrieve answer
    response <- knowledge_base[[platform]][[user_question]] %||% 
      "Sorry, I don't have an answer for that yet!"
    
    # Update chat log
    new_entry <- paste0(
      "You (", platform, "): ", user_question, "\n",
      "Bot: ", response
    )
    chat_log(paste(chat_log(), "\n", new_entry, sep = ""))
  })
  
  # Dynamic Question Update Based on Platform
  observeEvent(input$platform, {
    updateSelectInput(session, "predefined_question", choices = names(knowledge_base[[input$platform]]))
  })
  
  # Render Chat History
  output$chat_log <- renderText({
    chat_log()
  })
  
  
  
}

# ---- Run the App ----
shinyApp(ui, server)
