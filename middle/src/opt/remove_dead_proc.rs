// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use super::*;

#[derive(Default)]
pub struct RemoveDeadProc;

impl Pass for RemoveDeadProc {
    fn apply(&mut self, ctx: &mut Context) {
        let reachable = ctx.reachable_procs();

        ctx.procs.procs.retain(|proc, _| reachable.contains(proc));
    }
}
