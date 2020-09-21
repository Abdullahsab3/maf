package maf.modular.adaptive

import maf.core._
import maf.modular._
import maf.util.MonoidImplicits._

trait AdaptiveGlobalStore[Expr <: Expression] extends AdaptiveModAnalysis[Expr] 
                                                 with GlobalStore[Expr] {
  // update implementation for addresses and dependencies
  def updateAddr(update: Component => Component)(addr: Addr): Addr = ??? // TODO: implement
  override def updateDep(update: Component => Component)(dep: Dependency): Dependency = dep match {
    case AddrDependency(addr) => AddrDependency(updateAddr(update)(addr))
    case _ => super.updateDep(update)(dep)
  }
  // requires an implementation of alpha for the abstract domain
  def updateValue(update: Component => Component)(value: Value): Value
  // when abstraction map changes, need to update the store
  override def updateAnalysisData(update: Component => Component): Unit = {
    super.updateAnalysisData(update)
    val oldStore = store
    store = updateMap(updateAddr(update),updateValue(update))(store)
    // look if we have to retrigger any dependencies due to the abstraction
    oldStore.foreach { case (oldKey, oldValue) =>
      val newKey = updateAddr(update)(oldKey)
      val newValue = store(newKey)
      if (oldValue != newValue) {
        trigger(AddrDependency(newKey))
      }
    }
  }
}
