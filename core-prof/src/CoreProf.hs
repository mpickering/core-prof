module CoreProf (plugin) where
import GhcPlugins
import CostCentre

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  return (todo ++ [CoreDoPluginPass "CoreProf" coreProf])


coreProf :: ModGuts -> CoreM ModGuts
coreProf = bindsOnlyPass coreProf'

coreProf' :: CoreProgram -> CoreM CoreProgram
coreProf' cp = do
  mod <- getModule
  return $ map (addTick mod) cp



addTick :: Module -> Bind CoreBndr -> Bind CoreBndr
addTick mod (NonRec b r) = NonRec b (Tick tick r)
  where
    tick = ProfNote (mkAutoCC b mod) True True
addTick mod (Rec rs) = Rec (map go rs)
  where
    go (b, e) =
      let tick = ProfNote (mkAutoCC b mod) True True
      in (b, Tick tick e)



