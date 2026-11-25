const { chromium } = require('playwright');
(async () => {
  const b = await chromium.launch();
  const pg = await b.newPage({ viewport: { width: 1440, height: 1000 }, deviceScaleFactor: 1 });
  const errs = [];
  pg.on('console', m => { if (m.type() === 'error') errs.push(m.text()); });
  pg.on('pageerror', e => errs.push(String(e)));
  await pg.goto('http://127.0.0.1:8765');
  const idle = async () => { await pg.waitForTimeout(500); await pg.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy'), null, {timeout: 300000}); await pg.waitForTimeout(800); };
  await idle();
  const att = async () => (await pg.locator('#sim-boxes .value-box-value').nth(2).innerText());
  console.log('initial ATT', await att());
  await pg.screenshot({ path: 'shot_sim.png', fullPage: true });
  // change family to Clayton and push tau1 via shiny API, resimulate

  await pg.evaluate(() => $('#sim-family')[0].selectize.setValue('clayton'));
  await pg.evaluate(() => { $('#sim-tau1').data('ionRangeSlider').update({from: 0.7}); $('#sim-tau1').trigger('change'); });
  await pg.click('#sim-go'); await idle();
  console.log('clayton tau1=.7 ATT', await att());
  // three regimes
  await pg.check('input[name="sim-J"][value="3"]'); await pg.click('#sim-go'); await idle();
  console.log('J=3 boxes:', (await pg.locator('#sim-boxes').innerText()).replace(/\n/g,' | '));
  await pg.screenshot({ path: 'shot_sim3.png', fullPage: true });
  await pg.check('input[name="sim-J"][value="2"]'); await pg.click('#sim-go'); await idle();
  // estimation on simulator data
  await pg.click('a[data-value="est"]'); await idle();
  await pg.check('input[name="est-families"][value="clayton"]');
  await pg.click('#est-fit'); await idle();
  console.log('compare:', (await pg.locator('#est-compare_note').innerText())); console.log('pick:', await pg.locator('#est-pick').inputValue());
  console.log((await pg.locator('#est-compare table').last().innerText()).slice(0, 600));
  await pg.waitForTimeout(3000); await pg.screenshot({ path: 'shot_est.png', fullPage: true });
  await pg.click('#est-tabs a[data-value="mte"]'); await idle();
  await pg.screenshot({ path: 'shot_est_mte.png', fullPage: true });
  await pg.click('#est-tabs a[data-value="diag"]'); await idle();
  console.log((await pg.locator('#est-diag').innerText()).slice(0, 300));
  // PIAAC example, two-step
  await pg.check('input[name="est-source"][value="piaac"]'); await idle();
  await pg.check('input[name="est-method"][value="twostep"]');
  await pg.click('#est-fit'); await idle();
  await pg.click('#est-tabs a[data-value="compare"]'); await idle();
  console.log('piaac:', await pg.locator('#est-compare_note').innerText());
  // upload CSV
  await pg.check('input[name="est-source"][value="upload"]');
  await pg.setInputFiles('#est-file', 'upload.csv'); await idle();
  await pg.evaluate(() => { $('#est-y')[0].selectize.setValue('logwage'); $('#est-d')[0].selectize.setValue('ai');
    $('#est-x')[0].selectize.setValue(['skill']); $('#est-z')[0].selectize.setValue(['cost1','cost2']); });
  await idle();
  await pg.check('input[name="est-method"][value="ml"]');
  await pg.click('#est-fit'); await idle();
  console.log('upload:', await pg.locator('#est-compare_note').innerText());
  // results
  await pg.click('a[data-value="res"]'); await idle();
  await pg.screenshot({ path: 'shot_res_mc.png', fullPage: true });
  await pg.click('#res-tabs a[data-value="piaac"]'); await idle(); await pg.waitForTimeout(1500);
  await pg.screenshot({ path: 'shot_res_piaac.png', fullPage: true });
  for (const v of ['mc_tables','mc_fig','ms','log']) { await pg.click(`#res-tabs a[data-value="${v}"]`); await idle(); await pg.screenshot({ path: 'shot_res_'+v+'.png', fullPage: true }); }
  console.log('log head:', (await pg.locator('#res-piaac_txt').innerText()).slice(0,120));
  await pg.click('a[data-value="about"]'); await idle();
  const shinyErrs = await pg.locator('.shiny-output-error:visible').count();
  console.log('visible shiny errors on about:', shinyErrs, 'console errors:', errs);
  await b.close();
})();
