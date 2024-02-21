# RAFFLEIZE C-A-R-D-A-N-A  
### <br> (Commit-And-Reveal-Decentralized-Application-for-Native-Assets)
Version 1.1 approved
Prepared by Marius Georgescu  
IEEE System Requirements Specification Template


This project is part of
[F10-developer-ecosystem-the-evolution - Project Catalyst Idea #105248 ](https://projectcatalyst.io/funds/10/f10-developer-ecosystem-the-evolution/development-of-a-cardano-dapp-and-extensive-documentation-of-each-step-along-the-way)

Table of Contents
=================
- [RAFFLEIZE C-A-R-D-A-N-A](#raffleize-c-a-r-d-a-n-a)
    - [ (Commit-And-Reveal-Decentralized-Application-for-Native-Assets)](#-commit-and-reveal-decentralized-application-for-native-assets)
- [Table of Contents](#table-of-contents)
  - [Revision History](#revision-history)
  - [1.Introduction](#1introduction)
    - [1.1 Purpose](#11-purpose)
    - [1.2 Intended Audience and Reading Suggestions](#12-intended-audience-and-reading-suggestions)
    - [1.3 Scope](#13-scope)
      - [Objectives and Benefits](#objectives-and-benefits)
      - [Differentiators](#differentiators)
      - [Goals and Business Alignment](#goals-and-business-alignment)
    - [1.4 References](#14-references)
  - [2. Description](#2-description)
    - [2.1 Perspective](#21-perspective)
      - [Context and Origin](#context-and-origin)
      - [Relation to Larger Systems](#relation-to-larger-systems)
      - [System Overview and Interfaces](#system-overview-and-interfaces)
        - [User Interface Component](#user-interface-component)
        - [Wallet Component](#wallet-component)
        - [DApp Service Provider](#dapp-service-provider)
        - [Blockchain Network](#blockchain-network)
      - [High Level Architecture:](#high-level-architecture)
        - [Version 1: Centralized Hosting and Backend Transaction Handling](#version-1-centralized-hosting-and-backend-transaction-handling)
        - [Version 2: Centralized Hosting with Partial Decentralized Transaction Handling](#version-2-centralized-hosting-with-partial-decentralized-transaction-handling)
        - [Version 3: Decentralized Hosting and Transaction Handling](#version-3-decentralized-hosting-and-transaction-handling)
    - [2.2 Functionality](#22-functionality)
    - [Use-cases](#use-cases)
      - [NonAuthenticated User](#nonauthenticated-user)
      - [Authenticated User](#authenticated-user)
        - [Ticket Owner](#ticket-owner)
        - [Organizer](#organizer)
    - [2.3 Model](#23-model)
    - [2.4 States](#24-states)
      - [Ticket Token State Transitions](#ticket-token-state-transitions)
      - [Raffle Token State Transitions](#raffle-token-state-transitions)
      - [Raffle State Transitions](#raffle-state-transitions)
    - [Transactions](#transactions)
      - [Legend](#legend)
      - [Create Raffle](#create-raffle)
      - [Buy tickets to raffle](#buy-tickets-to-raffle)
      - [Update raffle configuration](#update-raffle-configuration)
      - [Cancel](#cancel)
      - [Collect Accumulated Amount](#collect-accumulated-amount)
      - [Recover Stake](#recover-stake)
      - [Recover Stake](#recover-stake-1)
      - [Collect collateral of unrevealed tickets](#collect-collateral-of-unrevealed-tickets)
      - [Reveal Secret](#reveal-secret)
      - [Collect Prize](#collect-prize)
      - [Full Refund](#full-refund)
      - [Extra Refund](#extra-refund)
      - [Collateral Refund](#collateral-refund)
    - [2.5 Design and Implementation Constraints](#25-design-and-implementation-constraints)
      - [Language Requirements](#language-requirements)
      - [Security Considerations](#security-considerations)
      - [Design Conventions or Programming Standards](#design-conventions-or-programming-standards)
    - [2.6 User Documentation](#26-user-documentation)
    - [2.7 Assumptions and Dependencies](#27-assumptions-and-dependencies)
      - [Assumptions](#assumptions)
      - [Dependencies](#dependencies)
  - [3. External Interface Requirements](#3-external-interface-requirements)
    - [3.1 User Interfaces](#31-user-interfaces)
      - [General User Interface Characteristics:](#general-user-interface-characteristics)
      - [Key Components of the User Interface:](#key-components-of-the-user-interface)
  - [](#)
  - [](#-1)
  - [](#-2)
    - [3.2 Software Interfaces](#32-software-interfaces)
  - [Appendix A: Glossary](#appendix-a-glossary)
      - [Definitions, Acronyms and Abbreviations](#definitions-acronyms-and-abbreviations)
        - [Acronyms and Abbreviations](#acronyms-and-abbreviations)
  - [4. Introduction](#4-introduction)




## Revision History
| Name | Date | Reason For Changes | Version |
| ---- | ---- | ------------------ | ------- |
|      |      |                    |         |
|      |      |                    |         |
|      |      |                    |         |

## 1.Introduction
📘  
### 1.1 Purpose 
💡  
This document describes the functional and non-functional requirements for a decentralized application ([DApp](#acronyms-and-abbreviations)) focused on digital assets raffles, based on a commit and reveal scheme ([CRS](#acronyms-and-abbreviations)) as source of randomness for electing the winner.  
The application described in this document will be further refered as [RAFFLEIZE C-A-R-D-A-N-A](#acronyms-and-abbreviations).  
This document covers only the application structure and behaviour and does not cover the consesnsus mechanism and communication protocol of the underlying blockchain.

### 1.2 Intended Audience and Reading Suggestions
👥  
This document serves as a comprehensive guide detailing the functionalities, scenarios, and acceptance criteria of the application. Intended for developers (to understand and implement the specified functionalities), testers (to create test cases and ensure the application meets the stipulated requirements), and other stakeholders. It provides a clear blueprint of how the application should function and behave under various circumstances.

### 1.3 Scope
🌐  
Designed to capitalize on the digital assets trend, [RAFFLEIZE C-A-R-D-A-N-A](#acronyms-and-abbreviations) is tailored for facilitating creation, management of, and participation in online raffles. 
This application allows users to raffle their digital assets, providing a chance for other users (participants) to win these assets in exchange for purchasing a ticket. By leveraging the commit and reveal scheme it decentralizes trust, ensuring transparency and fairness in the winner selection process.  

#### Objectives and Benefits
🎯  
- **Decentralized Trust**: The primary objective of [RAFFLEIZE C-A-R-D-A-N-A](#acronyms-and-abbreviations) is to eliminate the reliance on a central authority for trust. The commit and reveal scheme ensures that the process of picking a winner is transparent and verifiable, thus fostering trust among participants.

- **Digital Asset Monetization**: Users with digital assets can create raffles, offering others the chance to win these assets. This provides a unique avenue for asset owners to monetize their holdings.

- **Broadened Participation**: By allowing participants to buy tickets and potentially win digital assets, the platform democratizes access to these assets, which might be expensive or rare otherwise.

- **Enhanced Engagement**: The thrill of participating in a raffle and the potential to win valuable assets drive user engagement, encouraging more users to join the ecosystem. 

#### Differentiators 
💥💥💥
- **Enhanced Liquidity**: Tokenizing raffle ownership and tickets makes these assets more liquid. Owners can easily trade or sell their stakes or participation rights on various platforms, not limited to the original DApp. This increases the assets' marketability and potential value.
 
- **Recursive Raffle Opportunities**: (Raffles of Raffles) Owners can create raffles for their raffle stakes or tickets, adding a recursive layer of engagement. This not only multiplies the opportunities for users to engage with the platform but also introduces a novel concept of raffle chains, where one can win a stake in another raffle, thereby increasing the excitement and engagement levels.

- **Privacy Preservation**: Anonymity ensures that participants' and owners' identities are kept private, fostering a secure environment where users feel comfortable engaging in transactions. This privacy is particularly appealing to users who prioritize discretion in their online activities and financial dealings.

- **Inclusive Participation**: By removing the need for identity verification for participation, the platform can attract a wider audience, including those who, for various reasons, prefer or need to remain anonymous. This inclusivity strengthens the platform's market position by broadening its user base.

#### Goals and Business Alignment
[RAFFLEIZE C-A-R-D-A-N-A](#acronyms-and-abbreviations) aligns with the broader trend of decentralization and blockchain adoption. 

### 1.4 References
🔗  
https://github.com/cardano-foundation/CIPs/tree/master/CIP-0068  
https://github.com/cardano-foundation/CIPs/tree/master/CIP-0067  
https://github.com/cardano-foundation/CIPs/tree/master/CIP-0025   

## 2. Description
### 2.1 Perspective

[RAFFLEIZE C-A-R-D-A-N-A](#acronyms-and-abbreviations) is designed to fill a niche in the rapidly evolving digital asset landscape. However, its foundation and philosophy are deeply rooted in the broader shift towards decentralization and blockchain technology.

#### Context and Origin

While raffles have been a popular method of asset distribution and engagement for centuries, the digital age and rise of cryptocurrencies and non-fungible tokens ([NFTs](#acronyms-and-abbreviations)) have reshaped the potential of such events. [RAFFLEIZE C-A-R-D-A-N-A](#acronyms-and-abbreviations) is not merely a digital transformation of traditional raffles but a novel system that integrates the principles of decentralization.

#### Relation to Larger Systems

-  **Cardano Blockchain** : the current DApp, primarily relies on Cardano blockchain technology for it's core functionalities. The underlying blockchain provides the backbone of trust and decentralization and the underlying infrastructure that makes digital assets possible. 
- **IPFS**: Blockchain typically stores metadata that includes information about the digital asset, like an image, video, or piece of music. However, storing large files directly on the blockchain is impractical due to size limitations and high costs. [IPFS](#acronyms-and-abbreviations) offers a solution for decentralized storage. Instead of storing the actual data on blockchain, digital assets metadata often includes a URL or hash pointing to a file on [IPFS](#acronyms-and-abbreviations), utilizing its content addressing system to ensure the integrity and permanence of the data. 

#### System Overview and Interfaces

The diagram below highlights the key logical components of the DApp and their interactions. Each component plays a specific role in the functionality of the DApp, particularly within the context of a blockchain network. 

![Component Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/Component/ComponentDiagram-RaffleDApp-HighLevel.puml)

The user directly interacts with the UI, which is the entry point of the DApp. The UI facilitates user interaction with the blockchain through the Wallet component, which securely authorizes transactions. The DApp service provider acts as a bridge, handling complex interactions with the blockchain and ensuring smooth operation of the application. The blockchain network itself is the backbone, providing a decentralized and secure platform for executing the raffle's smart contracts.

Here's an elaborated description of each component:

##### User Interface Component

- **Role**: This component is the front-end o3f the DApp, where users interact with the application. It presents the user interface through which users can participate in raffles or organize their own.
- **Functions**: It manages user inputs, displays information (like raffle details and results), and initiates actions (like entering the raffle).
- **Interaction**: The UI communicates with the Browser Wallet to create transactions that will be sent to the blockchain.

##### Wallet Component
- **Role**: This is a third-party browser wallet that interacts with the DApp. It handles cryptographic functions related to the user's blockchain identity.
- **Functions**: Its primary function is to sign transactions, which is crucial for actions that require user authorization, such as entering a raffle or claiming rewards.
- **Interaction**: Receives transaction data from the UI, signs it with the user's private key, and then communicates it back to the UI or directly to the blockchain network.


##### DApp Service Provider
- **Role**: This backend component acts as the intermediary between the UI and the blockchain network.
- **Functions**:
  - Querying the Blockchain: It retrieves data from the blockchain, such as the status of the raffle, entries, and winners.
  - Constructing Transactions: It prepares transactions based on user actions that are to be executed on the blockchain.
- **Interaction**: It communicates with the blockchain network to fetch data or submit signed transactions.

##### Blockchain Network 
- **Role**: The underlying decentralized infrastructure that executes and records all operations related to the raffle.
- **Functions**: Hosts the smart contracts which govern the raffle logic, ensuring trustless and transparent execution. It processes transactions, updates the state of the raffle, and ensures the integrity and security of the entire process.
- **Interaction**: Interacts with the DApp service provider, receiving queries and transactions, and returning the results or confirmation of actions taken.


#### High Level Architecture:

There are three architectural versions envisaged for the DApp, each representing a different approach, varying by where the DApp static content is hosted, where transactions are constructed and how transactions are submited to the blockchain.


##### Version 1: Centralized Hosting and Backend Transaction Handling
- **Static Content Hosting**: The DApp's static content, like HTML, CSS, and JavaScript files, is hosted on a centralized web server.
- **Transaction Construction and Submission**: Transactions are constructed and submitted through the DApp's backend. The process involves:
  1. Users interacting with the UI, which calls an endpoint in the OffChain component.
  2. The OffChain component queries the Cardano node and constructs the transaction.
  3. The constructed transaction is sent to the user's browser wallet for signing.
  4. Once signed, the transaction is sent back to the OffChain component, which submits it to the Cardano node for final processing on the blockchain.


![Component Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/Component/ComponentDiagram-RaffleDApp-version1.puml) 

---
##### Version 2: Centralized Hosting with Partial Decentralized Transaction Handling
- **Static Content Hosting**: Similar to Version 1, the static content is hosted on a centralized server.
- **Transaction Construction**: The construction of transactions still happens in the DApp's backend.
- **Transaction Submission**: The signed transactions are submitted through the user's browser wallet or a custom node configured in the wallet. This adds a layer of decentralization to the process. *(Some wallets allow configuring a custom endpoint for submitting transactions, offering more flexibility and control to the user.)*

![Component Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/Component/ComponentDiagram-RaffleDApp-version2.puml)

---

##### Version 3: Decentralized Hosting and Transaction Handling
- **Static Content Hosting**: The DApp's static content is hosted on the IPFS network, offering a decentralized hosting solution.
- **Transaction Construction and Submission**:
    1.  The UI queries data from a Blockchain Services Provider.
    2. Transactions are constructed directly in the user's browser and sent to the browser wallet for signing.
    3. The signed transactions are then submitted to the blockchain network through the wallet's backend or a custom node, emphasizing decentralized processing.

![Component Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/Component/ComponentDiagram-RaffleDApp-version3.puml)

--- 


Each version offers different levels of decentralization and control:

- **Version 1** focuses on a more traditional web application structure with centralized control over transaction processing and content hosting.
- **Version 2** introduces an element of decentralization in transaction submission while keeping the content hosting and transaction construction centralized.
- **Version 3** fully embraces decentralization by leveraging IPFS for hosting and allowing transactions to be constructed and submitted directly from the user's browser, reducing reliance on a centralized backend.



### 2.2 Functionality

Below are summarized the major functions of the DApp, grouped based on the user classes:


### Use-cases

![Usecase Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/Usecase/UsecaseDiagram-RaffleDApp.puml)

#### NonAuthenticated User
- **Characteristics** : Represents users who have not connected a wallet. They have limited access to the DApp features compared to authenticated users.
- **Interactions** 
  - Can view active raffles.
  - Can connect to a wallet, which is a prerequisite for further interactions within the DApp.

#### Authenticated User
- **Characteristics** : Users who have successfully connected to a wallet. They have enhanced access and functionalities compared to non-authenticated users. This role forms the basis for more specialized roles like Participant and Organizer.
- **Interactions** :
  - Can view active raffles.
  - Can disconnect from the wallet.
  - View his own assets.
  - Create raffles.
  - Buy tickets for any of the active raffles.

##### Ticket Owner
- **Characteristics**  The owner of a ticket NFT for a raffle.
- **Interactions** 
  - View raffles joined and manage participation.
  - Redeem prize with winning ticket.
  - Recover collateral of losing ticket.
  - Get full refund of ticket price in case of underfunded raffle.
  - Get full refund of ticket price + extra share of unrevealed tickets value, in case of unrevealed raffle.

##### Organizer
- **Characteristics** : The owner of a raffle NFT .
- **Interactions** 
  - Update raffle configurations.
  - Cancel raffles.
  - Collect the accumulated amount for succesfully finalized raflles.
  - Recover the raffle stake in case of unsuccesfully finalized raffles (expired/underfunded/unrevealed).
  - Recover the raffle stake and the accumulated amount in the case none of the ticket participated in revealing phase.
  - Collect the ticket collateral of an unrevealed tickets for the owned raffle.



### 2.3 Model

![Class Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/Class/ClassDiagram-Raffleize.puml)

### 2.4 States


#### Ticket Token State Transitions

![Usecase Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/State/StateDiagram-TicketOwner_Token.puml)

#### Raffle Token State Transitions

![Usecase Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/State/StateDiagram-RaffleOwner_Token.puml)

#### Raffle State Transitions

![Usecase Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/State/StateDiagram-Raffle_State_Token.puml)


### Transactions

#### Legend
| Symbol        | Description          |
| ------------- | -------------------- |
| orange circle | UTxO                 |
| grey circle   | Reference UTxO       |
| 🎫👦🏻            | Raffle User NFT      |
| 🎫🔗            | Raffle Reference NFT |
| 🎟️👦🏻            | Ticket User NFT      |
| 🎟️🔗            | Ticket Reference NFT |
| 🔥             | Burning              |
| 🔨             | Minting              |
| 📝             | Datum                |
| 🧾             | Spending Validator   |
| 📜             | Minting Policy       |


---
#### Create Raffle
---
**Actors**: Authenticated User  
**Summary Description**:	This transaction allows anyone to create a raffle for some digital assets.  

**Preconditions:**
1. The organizer must own the raffle stake value.

**Postconditions:**
1. Raffle stake value, collateral and raffle reference NFT are locked to the raffle validator's address with valid datum. 


**Transaction:**

-TBD

![Mindmap Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/Mindmaps/1.Mindmap-U-CreateRaffle-Transaction.puml)


---
#### Buy tickets to raffle
---
**Actors**: Authenticated User  
**Summary Description**:	This transaction allows anyone to buy a ticket to an active raffle.  

**Preconditions:**
1. The raffle stake value must be locked to the raffle's validator address with valid datum.
2. User must have available funds to cover ticket price and ticket collateral.
3. The deadline for buying tickets must not have passed. 

**Postconditions:**
-TBD

**Transaction:**
-TBD

![Mindmap Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/Mindmaps/2.Mindmap-U-BuyTicket-Transaction.puml)

---
#### Update raffle configuration
---
**Actors**: Raffle Owner    
**Summary Description**:	This transaction allows the raffle owner to update the raffle configuration.  

**Preconditions:**
-TBD

**Postconditions:**
-TBD


**Transaction:**
-TBD

![Mindmap Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/Mindmaps/3.Mindmap-RO-UpdateRaffle-Transaction.puml)

---
#### Cancel
---
**Actors**: Raffle Owner    
**Summary Description**:	This transaction allows the raffle owner to cancel the raffle.  

**Preconditions:**
-TBD

**Postconditions:**
-TBD


**Transaction:**
-TBD


![Mindmap Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/Mindmaps/4.Mindmap-RO-CancelRaffle-Transaction.puml)


---
#### Collect Accumulated Amount
---
**Actors**: Raffle Owner    
**Summary Description**:	This transaction allows the raffle owner to collect the accumulated amount from a finalized raffle.

**Preconditions:**
-TBD

**Postconditions:**
-TBD


**Transaction:**
-TBD

![Mindmap Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/Mindmaps/5.Mindmap-RO-CollectAccumulatedAmount.puml)

---
#### Recover Stake
---
**Actors**: Raffle Owner    
**Summary Description**:	Recover the raffle stake in case of unsuccesfully finalized raffles (expired/underfunded/unrevealed).

**Preconditions:**
-TBD

**Postconditions:**
-TBD


**Transaction:**
-TBD

![Mindmap Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/Mindmaps/6.Mindmap-RO-RecoverStake-Transaction.puml)


#### Recover Stake
---
**Actors**: Raffle Owner    
**Summary Description**:	This transaction allows the raffle owner to recover the raffle stake and accumulated amount in the case none of the ticket owners revealed the ticket secret.

**Preconditions:**
-TBD

**Postconditions:**
-TBD


**Transaction:**
-TBD

![Mindmap Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/Mindmaps/7.Mindmap-RO-RecoverStakeAndAmount-Transaction.puml)


---
#### Collect collateral of unrevealed tickets
---
**Actors**: Raffle Owner    
**Summary Description**:	This transaction allows the raffle owner to collect the collateral of an unrevealed ticket.

**Preconditions:**
-TBD

**Postconditions:**
-TBD


**Transaction:**
-TBD


![Mindmap Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/Mindmaps/12.Mindmap-RO-GetCollateraOfExpiredTicket.puml)


---
#### Reveal Secret
---
**Actors**: Ticket Owner    
**Summary Description**:	This transaction allows to the ticket owner to reveal the ticket secret matching the ticket secrethash. 

**Preconditions:**
-TBD

**Postconditions:**
-TBD

**Transaction:**
-TBD

![Mindmap Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/Mindmaps/8.Mindmap-TO-RevealSecret-Transaction.puml)


---
#### Collect Prize
---
**Actors**: Ticket Owner    
**Summary Description**:	This transaction allows to the winner ticket owner to collect the raffle prize.  

**Preconditions:**
-TBD

**Postconditions:**
-TBD


**Transaction:**
-TBD

![Mindmap Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/Mindmaps/9.Mindmap-TO-CollectPrize-Transaction.puml)


---
#### Full Refund 
---
**Actors**: Ticket Owner    
**Summary Description**: This transaction allows the owner of a underfunded raffle ticket to get a full refund on ticket price and recover ticket collateral.  

**Preconditions:**
-TBD

**Postconditions:**
-TBD


**Transaction:**
-TBD

![Mindmap Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/Mindmaps/10.Mindmap-TO-FullRefund-Transaction.puml)

---
#### Extra Refund 
---
**Actors**: Ticket Owner    
**Summary Description**:	This transaction allows the owner of a revealed ticket in an unrevealed raffle to get a full refund on ticket price + his share of the value of the unrevealed tickets and recover ticket collateral.

**Preconditions:**
-TBD

**Postconditions:**
-TBD


**Transaction:**
-TBD

![Mindmap Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/Mindmaps/11.Mindmap-TO-ExtraRefund-Transaction.puml)

---
#### Collateral Refund 
---
**Actors**: Ticket Owner    
**Summary Description**: This transaction allows the owner of a reavealed losing ticket to get back the collateral.

**Preconditions:**
-TBD

**Postconditions:**
-TBD


**Transaction:**
-TBD

![Mindmap Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/Mindmaps/13.Mindmap-TO-CollateralRefund-Transaction.puml)






### 2.5 Design and Implementation Constraints
🖌️⛔   
#### Language Requirements

**Smart Contract Language:**
Use of PlutusTx for smart contracts. Consideration for alternative implementations in: Marlowe, Opshin, Aiken, Plutarch.   
**Frontend and Backend Development:**
Use functional programming languages for the DApp’s frontend and backend (e.g., Haskell, Purescript).

#### Security Considerations
**Smart Contract Security:** Audited and secure smart contracts to prevent vulnerabilities and exploits.  
**Testing:** Rigorous testing of the offchain code to prevent vulnerabilities.
#### Design Conventions or Programming Standards
**Code Quality and Maintainability:**
Adherence to industry-standard coding practices for readability, maintainability, and scalability.  
**Documentation:**
Comprehensive documentation for future maintenance and updates.  

### 2.6 User Documentation
📚  
List the user documentation components (such as user manuals, on-line help, and tutorials) that will be delivered along with the software. Identify any known user documentation delivery formats or standards.
1. **Step-by-Step Guides:**  Visual and textual guides for key functions and features.  
2. **Video tutorials:** Instructional videos demonstrating how to use the DApp
3. **Legal and Compliance Information:**
   1. Terms of Service: Legal terms and conditions of using the DApp.
   2. Privacy Policy: Information on data handling, user privacy, and security measures.

### 2.7 Assumptions and Dependencies
🤔 The development and successful operation of the RAFFLEIZE C-A-R-D-A-N-A DApp are contingent upon several assumptions and dependencies. Understanding these factors is crucial as they could significantly impact the project's trajectory and its requirements.
#### Assumptions
**Blockchain Stability and Accessibility:** The DApp assumes consistent uptime and stability of the Cardano blockchain network. Access to the blockchain is presumed uninterrupted, with minimal downtime.  
**Smart Contract Reliability:** It is assumed that the smart contracts,  will perform as intended without major bugs or vulnerabilities.  
**Regulatory Compliance:** The assumption that current and future regulations regarding blockchain technology, cryptocurrencies, and online gambling or raffles will remain favorable or at least not become prohibitively restrictive.  
**Technological Proficiency of Users:** A basic level of understanding and comfort with blockchain technology and digital wallets among the user base is assumed.  
**Third-Party Wallet Integration:** Seamless integration and compatibility with popular third-party browser wallets are assumed for transaction signing and user authentication.  
**Internet Connectivity:** Users are assumed to have stable and continuous access to the internet, essential for interacting with the blockchain and the DApp.
 

#### Dependencies
**Cardano Blockchain Infrastructure:** for it's core functionality the smart contract execution.  
**Browser wallets**: for user interactions with the blockchain, such as signing transactions and managing digital assets.  
**Blockchain Service Providers:** for user interactions with the blockchain.  
**Web Hosting Services:** For versions 1 and 2 of the DApp, dependency on centralized web hosting services. For version 3, reliance shifts to decentralized hosting solutions like IPFS.  


## 3. External Interface Requirements
🔌  
### 3.1 User Interfaces
💻  
#### General User Interface Characteristics:  
**Consistency:** The UI across the DApp maintains a consistent look and feel.   
**Responsive Design**: The UI adapts to different screen sizes and resolutions, providing an optimal experience on both desktop and mobile devices.   

#### Key Components of the User Interface:

--- 
- **Dashboard/Main Screen** 
  - **Layout:** A clean and intuitive layout displaying active raffles, with clear navigation menus for accessing other parts of the DApp.  
  - **Information Display:** Active raffles are presented with essential information like asset details, ticket price, and time remaining.  
  - **Interaction:** Easy access to join raffles or view more details with minimal clicks/taps.  
--- 
 
![Mindmap Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/Wireframes/UI-Dashboard-NonAuth.puml)


---
 
![Mindmap Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/Wireframes/UI-Dashboard-Auth.puml)
--- 

- **Raffle Detail Page**
  - **Information Display:**  Provides comprehensive information about a specific raffle state, including the asset being raffled, total number of participants, and the smart contract details.
  - **Interaction:**: A clear and secure way to purchase tickets for raffle.

--- 
![Mindmap Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/Wireframes/UI-RaffleDetails-Commit.puml) 
---
![Mindmap Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/Wireframes/UI-RaffleDetails-Reveal.puml) 
--- 

- **User Profile/Account Management**
  - **Information Display:** 
    - My Assets: Display current assets owned by the connected wallet.
    - My Tickets: Display current participations in raffles.
    - Transaction History: Display of past transactions and participations in raffles.
  - **Interaction:**  
    - Wallet Integration: Easy access for connecting or disconnecting digital wallets.
    - Create raffle: Possibility to create a raffle for a selected asset.
    - 
 
![Mindmap Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/Wireframes/UI-RaffleProfile-MyAssets.puml) 
![Mindmap Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/Wireframes/UI-RaffleProfile-MyTickets.puml) 

 

--- 

- **Raffle Creation Interface**
  - **Information Display:** Intuitive Forms: Simple forms to input details for creating a new raffle, such as asset details, ticket price, and duration.
  - **Interaction:**
    - Guidance and Help: Inline tips or help buttons to guide organizers through the raffle creation process.
    - Preview and Confirmation: Before submission, a preview of the raffle for review and confirmation.

 
![Mindmap Diagram](https://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/mariusgeorgescu/raffleize/main/Documentation/Diagrams/Wireframes/UI-RaffleProfile-Create.puml) 


--- 


- **Help and FAQ Sections**: Easily accessible help sections and FAQs for user assistance.
--- 


### 3.2 Software Interfaces
🖥️   


## Appendix A: Glossary
📄  
#### Definitions, Acronyms and Abbreviations

##### Acronyms and Abbreviations

| Acronym                 | Definition                                                                                                                                              |
| ----------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------- |
| DApp                    | Decentralized Application                                                                                                                               |
| RAFFLEIZE C-A-R-D-A-N-A | Commit-And-Reveal-Decentralized-Application-for-Native-Assets Raffles                                                                                   |
| CRS                     | Commit and Reveal Scheme                                                                                                                                |
| IPFS                    | InterPlanetary File System : is a protocol, hypermedia and file sharing peer-to-peer network for storing and sharing data in a distributed file system. |
| NFT                     | Non-Fungible Token                                                                                                                                      |



 


## 4. Introduction

This project uses the Nix package manager to build

direnv allow

nix build

nix run #cli

nix run #tests

nix run #cli

