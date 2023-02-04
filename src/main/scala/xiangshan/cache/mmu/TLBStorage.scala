/***************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *          http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  ***************************************************************************************/

package xiangshan.cache.mmu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.{ExtModule, chiselName}
import chisel3.util._
import utils._
import xiangshan.backend.fu.util.HasCSRConst

import scala.math.min

@chiselName
class TLBFakeSP(
             ports: Int,
             nSets: Int,
             nWays: Int,
             useDmode: Boolean = false
           )(implicit p: Parameters) extends TlbModule with HasCSRConst{

  val io = IO(new TlbStorageIO(nSets, nWays, ports))
  io.r.req.map(_.ready := true.B)
  val mode = if (useDmode) io.csr.priv.dmode else io.csr.priv.imode
  val vmEnable = if (EnbaleTlbDebug) (io.csr.satp.mode === 8.U)
    else (io.csr.satp.mode === 8.U && (mode < ModeM))

  for (i <- 0 until ports) {
    val req = io.r.req(i)
    val resp = io.r.resp(i)
    io.r.resp_hit_sameCycle(i) := true.B

    val helper = Module(new PTEHelper())
    helper.clock := clock
    helper.satp := io.csr.satp.ppn
    helper.enable := req.fire && vmEnable
    helper.vpn := req.bits.vpn

    val pte = helper.pte.asTypeOf(new PteBundle)
    val ppn = pte.ppn
    val vpn_reg = RegNext(req.bits.vpn)
    val pf = helper.pf
    val level = helper.level

    resp.valid := RegNext(req.valid)
    resp.bits.hit := true.B
    resp.bits.perm(0).pf := pf
    resp.bits.perm(0).af := false.B
    resp.bits.perm(0).d := pte.perm.d
    resp.bits.perm(0).a := pte.perm.a
    resp.bits.perm(0).g := pte.perm.g
    resp.bits.perm(0).u := pte.perm.u
    resp.bits.perm(0).x := pte.perm.x
    resp.bits.perm(0).w := pte.perm.w
    resp.bits.perm(0).r := pte.perm.r
    resp.bits.perm(0).pm := DontCare

    resp.bits.ppn(0) := MuxLookup(level, 0.U, Seq(
      0.U -> Cat(ppn(ppn.getWidth-1, vpnnLen*2), vpn_reg(vpnnLen*2-1, 0)),
      1.U -> Cat(ppn(ppn.getWidth-1, vpnnLen), vpn_reg(vpnnLen-1, 0)),
      2.U -> ppn)
    )
  }

  io.access := DontCare
  io.victim.out := DontCare

}

@chiselName
class TLBFakeNP(
             ports: Int,
             nDups: Int,
             nSets: Int,
             nWays: Int
           )(implicit p: Parameters) extends TlbModule {

  val io = IO(new TlbStorageIO(nSets, nWays, ports, nDups))

  io.r.req.map(_.ready := true.B)
  io.r.resp_hit_sameCycle := DontCare
  io.r.resp := DontCare
  io.access := DontCare
  io.victim.out := DontCare
}

object TlbStorage {
  def apply
  (
    name: String,
    associative: String,
    sameCycle: Boolean,
    ports: Int,
    nDups: Int = 1,
    nSets: Int,
    nWays: Int,
    saveLevel: Boolean = false,
    normalPage: Boolean,
    superPage: Boolean,
    useDmode: Boolean
  )(implicit p: Parameters) = {
      if (superPage == true) {
        val storage = Module(new TLBFakeSP(ports, nSets, nWays, useDmode))
        storage.io
      } else {
        val storage = Module(new TLBFakeNP(ports, nDups, nSets, nWays))
        storage.io
      }
  }
}
