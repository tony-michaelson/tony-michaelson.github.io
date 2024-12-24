---
title: 'Solana Stablecoin'  
date: 2024-11-7 00:00:00  
featured_image: '/images/portfolio/solana.webp'
subtitle: A zero liquidation stable coin lending protocol
---

I was hired to produce a very sophisticated high precision blockchain project to deliver a lending protocol on Solana based on the naitive token as the collateral source and ensure loans would not be liquidated. The protocal design we arrived at leveraged a protocol token liquidity pool as a buffer for defending the peg instead of customer collateral. The design was intended to simultaneously protect user collateral and prevent the dramatic sell offs of native tokens happening on various blockchains during mass liquidation events which has made the problem worse and manipulation easier.

The system was entired decentralized including the Oracle system we custom built to avoid off chain price sources. We built a bond system for selling our protocol token and raising capital to populate our liquidity pools and boot the lending contracts.

The project took 18 months and we only had two engineers to build it on a small budget. We successfully completed and shipped the product according to the specifications provided and solved every complex challenge along the way. The end product passed an initial audit by Neodyme. Our investor pulled his funding on the day we delivered the finalized product due to concerns over the Securities and Exchange Commission.

You can see the code for the project here:

[Rust Source Code](https://github.com/tony-michaelson/solana-stablecoin-contracts)

[Typescript/React UI](https://github.com/tony-michaelson/solana-stablecoin-dapp-ui)

---

### **Protocol Design**

The arbitrage mechanisms in this program are designed to maintain price stability for the stable coin token and optimize liquidity across various Automated Market Makers (AMMs) like **Raydium**, **Orca**, and **Saber**.

---

### **Summary of Lending Functionality**

The lending functionality in this protocol allows users to obtain loans in the `stablecoin` token while ensuring **zero liquidation risk**. Loans are backed by SOL collateral, and the protocol includes safeguards to maintain solvency and incentivize repayment. Here’s how the lending system works:

---

### **1. How Loans Are Opened**

#### **Loan Opening Process:**
- A user deposits collateral into the protocol.
- The user specifies the loan amount in `stablecoin` they wish to borrow.
- The protocol calculates whether the user's collateral meets the **minimum collateral requirement** (collateral ratio).

#### **Key Functions:**
- `process_open_loan`: 
  - Verifies collateral value using price oracles (e.g., `SOL_USDC_ORACLE`, `SOL_USDT_ORACLE`).
  - Ensures the requested loan amount respects the collateral ratio.
  - Mints `protocol` tokens for the user and assigns the loan to their account.

#### **Loan Initialization:**
- Loans are tracked through dedicated **loan accounts** that store:
  - Collateral amount.
  - Borrowed `protocol` amount.
  - Loan metadata (timestamps, status, etc.).

---

### **2. Guaranteeing Zero Liquidation**

#### **Mechanism to Avoid Liquidation:**
- **Overcollateralization:** 
  - Users must deposit more collateral than the value of the loan to guarantee the protocol’s solvency.
  - The collateral ratio is strictly enforced during loan opening and maintained throughout the loan's lifetime.
- **Peg Stability Mechanism:**
  - Arbitrage and price stability mechanisms keep the value of `protocol` aligned with its intended peg.
  - This reduces risks of undercollateralization due to sudden price fluctuations.

#### **No Margin Calls or Liquidations:**
- Since the protocol ensures users cannot borrow beyond their collateral limit, loans remain fully backed at all times.
- Even if the market value of the collateral fluctuates, the collateral ratio guarantees the protocol's solvency.

---

### **3. Terms of the Loan**

#### **Loan Parameters:**
- **Collateral Types:**
  - Supported assets (e.g., SOL) determined by governance.
- **Interest-Free Loans:**
  - Borrowers do not accrue interest on their loans.
  - The protocol monetizes through minting fees or transaction fees instead.
- **Repayment Terms:**
  - Borrowers can repay the loan at any time to recover their collateral.
- **Maximum Loan Amount:**
  - Determined by the collateral’s market value and the protocol’s **collateral requirement** (e.g., 150% collateral ratio).

#### **Loan Tracking:**
- Each loan is linked to an individual user account.
- Loans are categorized as active until the full `protocol` amount is repaid.

---

### **4. Loan Management**

#### **Repayment:**
- Borrowers repay `protocol` tokens to recover their collateral.
- The repayment process involves burning the borrowed `protocol` from the user’s account.

#### **Collateral Adjustment:**
- Borrowers can increase their collateral to improve their collateral ratio.
- Partial repayments allow users to reduce their debt while keeping the loan active.

#### **Loan Closure:**
- Loans are closed once the borrower repays the full borrowed amount.
- Collateral is returned to the user’s account.

---

### **5. Advantages of Using the Protocol**

#### **Zero Liquidation Risk:**
- No forced liquidation of collateral, even during market volatility.
- Borrowers have peace of mind knowing their collateral is safe.

#### **Interest-Free Loans:**
- Users can borrow `protocol` without accruing interest, making it cost-effective.

#### **Peg Stability:**
- The protocol’s arbitrage mechanisms ensure the stability of `protocol`, minimizing risks associated with price volatility.

#### **Transparent and Decentralized:**
- All loan parameters and transactions are recorded on-chain, ensuring transparency and trust.

#### **Flexible Loan Management:**
- Users can repay loans at their convenience and adjust collateral levels as needed.

#### **Governance Control:**
- The DAO can adjust loan parameters (e.g., collateral ratio, supported assets) to adapt to market conditions and maintain system stability.

---

### **Key Components of Arbitrage Mechanism**

1. **Liquidity Pools and AMMs**
   - The program interacts with multiple AMMs to execute swaps between tokens (`protocol`, `stablecoin`, SOL, etc.).
   - It uses specific liquidity pools for arbitrage opportunities:
     - **Raydium**: A decentralized AMM on Solana.
     - **Orca**: Another AMM focusing on user-friendly liquidity provision.
     - **Saber**: A decentralized stable coin liquidity protocol.

2. **Price Oracles**
   - The program fetches real-time price data from oracles (e.g. `SOL_USDC_ORACLE`, `SOL_USDT_ORACLE`, `protocol_SOL_ORACLE`) to determine whether there are arbitrage opportunities.

3. **Arbitrage Types**
   - **Minting for Arbitrage:** Mint new `protocol` or `stablecoin` tokens when prices indicate a potential arbitrage opportunity and pool conditions allow it.
   - **Selling for Arbitrage:** Sell tokens in specific AMMs to stabilize prices or generate profit.

4. **State Tracking**
   - The system maintains a global arbitrage state (`ArbState`) that tracks:
     - Daily arbitrage limits.
     - Maximum `protocol` to mint.
     - Pool imbalances and pending arbitrage actions.

---

### **How Arbitrage Works**

#### 1. **Detecting Arbitrage Opportunities**
- **Price Comparison:**
  - The program fetches prices from oracles for different token pairs.
  - It calculates the price disparity between tokens in different AMMs (e.g. `protocol/SOL` on Raydium vs. Orca).
  - If a significant price difference exists, it initiates an arbitrage process.

- **Volume Analysis:**
  - Verifies whether the AMM pools have sufficient liquidity to execute the arbitrage without excessive slippage.
  - Uses custom checks (`verify_raydium_pools_are_balanced`, `verify_orca_pools_are_balanced`) to ensure liquidity.

#### 2. **Minting for Arbitrage**
- **Minting Conditions:**
  - If liquidity is imbalanced, the program mints new `protocol` or `stablecoin` tokens to inject into the pool with lower liquidity.
  - Minting occurs only if the arbitrage state allows it (e.g. daily minting limits are not exceeded).

- **Pool Balancing:**
  - The minted tokens are swapped for the underlying asset (e.g. SOL) in the AMM where liquidity is insufficient.
  - Balances the pool while generating profit or stabilizing prices.

#### 3. **Selling for Arbitrage**
- **Selling Conditions:**
  - If the pool has an excess of a token (`protocol` or `stablecoin`), the program sells the surplus to another AMM or converts it back to the base asset (e.g. SOL).


- **Price Stabilization:**
  - Reduces the surplus token supply in one pool and transfers it to another pool with higher demand, stabilizing prices across AMMs.

#### 4. **Balancing Pools**
- **Pool Verifications:**
  - After executing an arbitrage action, the program verifies whether the pools have returned to a balanced state using price and liquidity data.
  - If pools remain imbalanced, additional actions may be taken.

#### 5. **Rewarding Arbitrage Participants**
- Users or entities that trigger arbitrage actions (e.g. submitting transactions or updating pool data) are rewarded with `protocol` tokens.
- Rewards are proportional to the effort, incentivizing system participation.

---

### **Key Functions in Arbitrage**

#### **Minting Functions**
- `mint_protocol_for_arb_checking_orca` and `mint_protocol_for_arb_checking_raydium`:
  - Mints `protocol` tokens based on liquidity conditions in Orca or Raydium pools.
- `mint_stablecoin_for_arb_checking_orca` and `mint_stablecoin_for_arb_checking_raydium`:
  - Mints `stablecoin` tokens for arbitrage purposes.

#### **Selling Functions**
- `sell_protocol_for_arb_funds_using_orca` and `sell_protocol_for_arb_funds_using_raydium`:
  - Sells `protocol` tokens in Orca or Raydium pools for SOL or other assets.
- `sell_stablecoin_for_arb_funds_using_orca` and `sell_stablecoin_for_arb_funds_using_raydium`:
  - Sells `stablecoin` tokens in AMMs to balance pools and stabilize prices.

#### **Validation Functions**
- `verify_raydium_pools_are_balanced` and `verify_orca_pools_are_balanced`:
  - Ensure that the arbitrage actions do not create significant imbalances in the pools.

---

### **Constraints**
- **Daily Limits:** Arbitrage actions are limited by a daily cap to prevent abuse or destabilization of the ecosystem.
- **Oracle Dependence:** Relies on accurate oracle data; incorrect data could lead to erroneous decisions.
- **Governance Control:** The DAO sets limits on minting, selling, and arbitrage parameters to maintain system stability.

---

### **Benefits**
- **Price Stability:** Ensures consistent pricing of the `protocol` token across different platforms.
- **Efficient Liquidity:** Optimizes token distribution across AMMs for better liquidity.
- **Ecosystem Incentivization:** Rewards users for maintaining system health and participating in arbitrage.

---
