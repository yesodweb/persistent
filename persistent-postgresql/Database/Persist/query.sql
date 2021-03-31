INSERT INTO (
  currencycloud_balances
) VALUES (
  externalAccountID, 
  externalAmount
) ON CONFLICT DO UPDATE SET (
  amount = externalAmount
) WHERE amount != externalAmount
