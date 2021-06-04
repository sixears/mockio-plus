--------------------------------------------------------------------------------

{- | Log a mockable IO Action, including its result (if provided a suitable
     formatter), and any exception it throws. -}
mkIOLMER ∷ (MonadIO μ, Printable ε, MonadError ε μ, HasCallStack,
            MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
            Severity → IOClass → 𝕋 → 𝕄 (α → [𝕋]) → α
         → ExceptT ε IO α → DoMock → μ α
mkIOLMER sev ioclass msg valmsg mock_value io mck = do
  let stg  = def & ioClass ⊢ ioclass & doMock ⊢ mck
      pp ∷ DoMock → 𝕋 → 𝕋
      pp NoMock t = t
      pp DoMock t = "(" ⊕ t ⊕ ")"
  result ← mkIOL sev ioclass msg (Right mock_value) (ѥ io) mck
  case result of
    Left  e → do logIO sev stg (pp mck $ [fmtT|%t FAILED: %T|] msg e)
                 throwError e
    Right r → do case valmsg of
                   Nothing → return ()
                   Just v  → forM_ (v r) $ \ t →
                     logIO sev stg (pp mck $ [fmtT|%t: %t|] msg t)
                 return r

-- that's all, folks! ----------------------------------------------------------

