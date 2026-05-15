# Changelog

## [0.2.0](https://github.com/hshn/yoshi/compare/v0.1.0...v0.2.0) (2026-05-15)


### Features

* add contramap to Validation ([#85](https://github.com/hshn/yoshi/issues/85)) ([eddc9fa](https://github.com/hshn/yoshi/commit/eddc9fae9fd55ca17eb5ea2bb59f722406c56247))
* add documentation site with mdoc + Docusaurus ([#102](https://github.com/hshn/yoshi/issues/102)) ([b621593](https://github.com/hshn/yoshi/commit/b621593b789c323c9cef53ba17d097c429b629b6))
* add mapError to Validation ([#89](https://github.com/hshn/yoshi/issues/89)) ([5b8ec2f](https://github.com/hshn/yoshi/commit/5b8ec2f56c4ca139ee73f6fc03b8262e549a507a))
* add numeric validators (min, max, positive) ([#93](https://github.com/hshn/yoshi/issues/93)) ([56db66b](https://github.com/hshn/yoshi/commit/56db66b964fd0500fa638f656160f7ffbfaf4d89))
* add optional, Option/Seq auto-derivation for Validation ([#88](https://github.com/hshn/yoshi/issues/88)) ([1e6f0ff](https://github.com/hshn/yoshi/commit/1e6f0ff08a00ff62f95ed71f9a4eb096a90bc690))
* add orElse for fallback validation ([#90](https://github.com/hshn/yoshi/issues/90)) ([9792cc7](https://github.com/hshn/yoshi/commit/9792cc759b549905bd8dc8cf62537f7d4ffe2fe6))
* add Validation.cursor API with compile-time field name extraction ([#100](https://github.com/hshn/yoshi/issues/100)) ([d9a1d6e](https://github.com/hshn/yoshi/commit/d9a1d6e2d91a66b13257736e4b292efe17ae16fd))
* add Validation.succeed and Validation.fail ([#83](https://github.com/hshn/yoshi/issues/83)) ([c03e320](https://github.com/hshn/yoshi/commit/c03e3201e9b0839daefb761e09c405d90dc90b6c))
* add Violations.toList for tree flattening ([#87](https://github.com/hshn/yoshi/issues/87)) ([9da99fb](https://github.com/hshn/yoshi/commit/9da99fb8a854fea53561056bb399b33348de2f83))
* add zio-prelude module ([#97](https://github.com/hshn/yoshi/issues/97)) ([039dbc0](https://github.com/hshn/yoshi/commit/039dbc00c268b6f6fe6bb12950ef4586a7080962))
* initial implmentation ([#1](https://github.com/hshn/yoshi/issues/1)) ([be08589](https://github.com/hshn/yoshi/commit/be08589e93146551438ac7095399782cc991b0ff))
* parallelize validateN execution ([#57](https://github.com/hshn/yoshi/issues/57)) ([#81](https://github.com/hshn/yoshi/issues/81)) ([c13c0e9](https://github.com/hshn/yoshi/commit/c13c0e91d465be8efd49117441a83c4da5c1bb24))
* remove andValidate, keep &gt;&gt; for sequential composition ([#64](https://github.com/hshn/yoshi/issues/64)) ([#84](https://github.com/hshn/yoshi/issues/84)) ([01dfc44](https://github.com/hshn/yoshi/commit/01dfc441e7d438e5eb7d235aea4d8da10609c917))
* replace TupleOperationGenerator with typeclass-based tuple validation ([#76](https://github.com/hshn/yoshi/issues/76)) ([d9eb931](https://github.com/hshn/yoshi/commit/d9eb9310559f806ff724bcdc869e57a4ba9b744a))
* replace ValidateAsPartiallyApplied with ValidatedAs bridge typeclass ([#96](https://github.com/hshn/yoshi/issues/96)) ([5c13cb0](https://github.com/hshn/yoshi/commit/5c13cb09367b6f44cbcb4dbb852ad1556b4ef91d))
* replace ViolationFactory with direct error parameters ([#86](https://github.com/hshn/yoshi/issues/86)) ([8eb3e22](https://github.com/hshn/yoshi/commit/8eb3e223f08cd8a5a5ec15c769779cd25e3db622))
* seal Validation class and hide constructor ([#80](https://github.com/hshn/yoshi/issues/80)) ([01070e7](https://github.com/hshn/yoshi/commit/01070e79897e8d607ce775234532dd7857269503))
* sort violations by path for deterministic toList output ([#120](https://github.com/hshn/yoshi/issues/120)) ([b149335](https://github.com/hshn/yoshi/commit/b149335bd40c61a7011becb5f3a01747023e13aa))
* upgrade to scala3 ([#74](https://github.com/hshn/yoshi/issues/74)) ([d4fb8f0](https://github.com/hshn/yoshi/commit/d4fb8f06b5c716d6ee9b80697d63a2a46e92cb13))


### Bug Fixes

* avoid docusaurusCreateSite redirect overwriting index.html ([#108](https://github.com/hshn/yoshi/issues/108)) ([b4f0056](https://github.com/hshn/yoshi/commit/b4f0056d834ccc71ae120b7e4dadb95844c0ec2f))
* **ci:** force Scala version switch with ++! ([#110](https://github.com/hshn/yoshi/issues/110)) ([19b9b5b](https://github.com/hshn/yoshi/commit/19b9b5b7a46b8b4e18fbe9d375d9a1f61e8e9a99))
* **deps:** update react monorepo to v19 ([#107](https://github.com/hshn/yoshi/issues/107)) ([f88698e](https://github.com/hshn/yoshi/commit/f88698e9f7c2b06de760c1c6fcd25ca2a645df15))
* make |+| left-biased instead of throwing RuntimeException ([#77](https://github.com/hshn/yoshi/issues/77)) ([aabcf20](https://github.com/hshn/yoshi/commit/aabcf2055ef7932dc697c4fc3623a13c2b5f5d95))
* use blacklist approach for renovate minor automerge ([#55](https://github.com/hshn/yoshi/issues/55)) ([6d62ffe](https://github.com/hshn/yoshi/commit/6d62ffe61444dbf75885d1b9b7363590ead0527f))
* use correct package name for Scala in Renovate config ([#113](https://github.com/hshn/yoshi/issues/113)) ([ddd8df2](https://github.com/hshn/yoshi/commit/ddd8df2aa2eae39277b9767eb5559f05d9d9d85f))
