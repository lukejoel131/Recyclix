# 🌱 Recyclix - Smart Recycling Incentives

> Reward proper disposal with on-chain tokens! ♻️

## 📋 Overview

Recyclix is a blockchain-based recycling incentive system that rewards users with RCX tokens for properly disposing of recyclable materials. Users submit their recycling activities, authorized verifiers confirm the submissions, and tokens are automatically distributed as rewards.

## ✨ Features

- 🪙 **Token Rewards**: Earn RCX tokens for verified recycling submissions
- 📊 **Material Categories**: Support for plastic, glass, metal, paper, and electronics
- ✅ **Verification System**: Authorized verifiers ensure submission authenticity
- 📈 **User Statistics**: Track your recycling impact and token earnings
- 🔄 **Token Transfer**: Send tokens to other users
- ⚙️ **Configurable Rates**: Admin can adjust reward rates per material type

## 🚀 Getting Started

### Prerequisites
- Clarinet installed
- Stacks wallet for testing

### Installation

```bash
clarinet new recyclix-project
cd recyclix-project
```

Copy the contract code to `contracts/recyclix.clar`

### 🔧 Contract Functions

#### Admin Functions
- `initialize-contract()` - Set up initial token supply and material rates
- `add-verifier(verifier)` - Add authorized verifier
- `remove-verifier(verifier)` - Remove verifier authorization
- `set-material-rate(material-type, rate)` - Update reward rates

#### User Functions
- `submit-recycling(material-type, weight-kg)` - Submit recycling activity
- `transfer-tokens(amount, recipient)` - Transfer tokens to another user

#### Verifier Functions
- `verify-submission(submission-id)` - Verify and reward recycling submission

#### Read-Only Functions
- `get-token-balance(user)` - Check user's token balance
- `get-submission(submission-id)` - Get submission details
- `get-user-stats(user)` - Get user's recycling statistics
- `get-material-rate-info(material-type)` - Check reward rates

## 💰 Material Reward Rates

| Material | Tokens per KG |
|----------|---------------|
| 🥤 Plastic | 100 RCX |
| 🍾 Glass | 150 RCX |
| 🔩 Metal | 200 RCX |
| 📄 Paper | 75 RCX |
| 💻 Electronics | 500 RCX |

## 🎯 Usage Example

1. **Initialize the contract** (admin only)
2. **Add verifiers** to validate submissions
3. **Users submit recycling** with material type and weight
4. **Verifiers confirm** legitimate submissions
5. **Tokens are automatically minted** to user's wallet
6. **Users can transfer tokens** or track their impact

## 🧪 Testing

```bash
clarinet test
```

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## 📄 License

This project is open source and available under the MIT License.

---

**Start recycling, start earning! 🌍💚**


