import Mathlib.Topology.Basic
import Mathlib.Topology.Separation
import Mathlib.Topology.Bases
import Mathlib.Topology.MetricSpace.Basic
import Mathlib.Analysis.NormedSpace.Basic
import Mathlib.Analysis.InnerProductSpace.PiL2
import Mathlib.Analysis.Normed.Module.Ball.Homeomorph

noncomputable section

open Set TopologicalSpace Metric Filter

/-!
# Chapter 1: Smooth Manifolds

We define Topological Manifolds and prove Lemma 1.10.
-/

variable (M : Type*) [TopologicalSpace M]
variable (n : ℕ)

-- We use `Fin n → ℝ` to represent ℝⁿ.

/--
**Definition:** M is locally Euclidean of dimension n if each point of M has a neighborhood
that is homeomorphic to an open subset of ℝⁿ.
-/
def IsLocallyEuclidean : Prop :=
  ∀ p : M, ∃ (U : Set M) (V : Set (Fin n → ℝ)) (φ : U ≃ₜ V),
    IsOpen U ∧ p ∈ U ∧ IsOpen V

/--
**Definition:** Variation where the open set is a ball.
-/
def IsLocallyEuclideanBall : Prop :=
  ∀ p : M, ∃ (U : Set M) (x : Fin n → ℝ) (r : ℝ) (φ : U ≃ₜ Metric.ball x r),
    IsOpen U ∧ p ∈ U

/--
**Definition:** Variation where the open set is ℝⁿ.
-/
def IsLocallyEuclideanRn : Prop :=
  ∀ p : M, ∃ (U : Set M) (φ : U ≃ₜ (Set.univ : Set (Fin n → ℝ))),
    IsOpen U ∧ p ∈ U

/--
**Exercise 1.1:** Equivalent definitions of manifolds.
-/
theorem exercise_1_1 :
    (IsLocallyEuclidean M n ↔ IsLocallyEuclideanBall M n) ∧
    (IsLocallyEuclidean M n ↔ IsLocallyEuclideanRn M n) := by
  constructor
  · constructor
    · intro h p
      obtain ⟨U, V, φ, hU, hp, hV⟩ := h p
      -- V is open, so it contains a ball around φ(p)
      let y := φ ⟨p, hp⟩
      obtain ⟨ε, hε, h_ball_in⟩ := Metric.isOpen_iff.mp hV y (by simp)
      let V' := Metric.ball (y : Fin n → ℝ) ε
      let U' := φ ⁻¹' V'
      use U', (y : Fin n → ℝ), ε
      -- We need to construct the homeomorphism U' ≃ V'
      let φ' : U' ≃ₜ V' := φ.preimageHomeomorph V'
      use φ'
      constructor
      · exact φ.continuous_invFun.isOpen_preimage V' Metric.isOpen_ball
      · simp [U', V', y]
    · intro h p
      obtain ⟨U, x, r, φ, hU, hp⟩ := h p
      use U, Metric.ball x r, φ
      refine ⟨hU, hp, Metric.isOpen_ball⟩
  · constructor
    · intro h p
      -- This direction requires showing any point has a neighborhood homeomorphic to R^n.
      -- A ball is homeomorphic to R^n.
      obtain ⟨U, x, r, φ, hU, hp⟩ := (exercise_1_1.1.mp h) p
      -- We know U is non-empty since p ∈ U, so the ball is non-empty, implying r > 0.
      have hr : 0 < r := by
        have : φ p ∈ Metric.ball x r := φ.map_source.mp hp
        rw [Metric.mem_ball] at this
        linarith [dist_nonneg (φ p) x]
      
      -- Ball ≃ R^n
      let ball_to_Rn : Metric.ball x r ≃ₜ (Set.univ : Set (Fin n → ℝ)) :=
        (Homeomorph.univBall x r hr).symm
      
      -- We need to compose φ : U ≃ₜ ball with ball_to_Rn
      let φ' := φ.trans ball_to_Rn
      use U, φ'
      refine ⟨hU, hp, isOpen_univ⟩
    · intro h p
      obtain ⟨U, φ, hU, hp⟩ := h p
      use U, Set.univ, φ
      refine ⟨hU, hp, isOpen_univ⟩

/--
**Theorem 1.2 (Topological Invariance of Dimension):**
A nonempty n-dimensional topological manifold cannot be homeomorphic to an m-dimensional manifold unless m = n.
-/
theorem theorem_1_2 {m : ℕ} [TopologicalManifold M n] [TopologicalManifold M m] [Nonempty M] :
    (Nonempty (M ≃ₜ M)) → m = n := by
  intro h_homeo
  -- Requires Invariance of Domain / Homology
  sorry

/--
**Definition:** A Topological Manifold is a Hausdorff, Second-Countable, Locally Euclidean space.
-/
class TopologicalManifold extends T2Space M, SecondCountableTopology M : Prop where
  locally_euclidean : IsLocallyEuclidean M n

variable {M n}

/--
**Definition:** A Coordinate Ball is an open subset U of M that is homeomorphic to an open ball in ℝⁿ.
(Lee defines it as the domain of a chart whose image is an open ball).
-/
def IsCoordinateBall (U : Set M) : Prop :=
  IsOpen U ∧ ∃ (x : Fin n → ℝ) (r : ℝ) (φ : U ≃ₜ Metric.ball x r), True

/--
**Lemma 1.10:** Every topological manifold has a countable basis of precompact coordinate balls.
-/
theorem lemma_1_10 [TopologicalManifold M n] :
    ∃ (B : Set (Set M)),
      B.Countable ∧
      IsTopologicalBasis B ∧
      (∀ U ∈ B, IsCoordinateBall n U) ∧
      (∀ U ∈ B, IsCompact (closure U)) := by
  -- "Let M be a topological n-manifold." (Assumed by typeclass)
  
  -- "First we consider the special case in which M can be covered by a single chart."
  -- We formalize the general case directly which subsumes this, following the second paragraph of Lee's proof.
  
  -- "By definition, each point of M is in the domain of a chart."
  have h_cover : ∀ p : M, ∃ U, IsOpen U ∧ p ∈ U ∧ IsCoordinateBall n U := by
    intro p
    -- From locally euclidean property
    obtain ⟨U_large, V_large, φ, hU_open, hp_in, hV_open⟩ := TopologicalManifold.locally_euclidean p
    -- V_large is open in R^n, so it contains a ball B(y, ε) around φ(p)
    let y := φ ⟨p, hp_in⟩
    obtain ⟨ε, hε, h_ball_in⟩ := Metric.isOpen_iff.mp hV_open y (by simp)
    -- The preimage of this ball is a coordinate ball.
    let V := Metric.ball (y : Fin n → ℝ) ε ∩ V_large
    -- Since Metric.ball y ε ⊆ V_large, V = Metric.ball y ε
    have hV_eq : Metric.ball y ε ∩ V_large = Metric.ball y ε := Set.inter_eq_self_of_subset_left h_ball_in
    let U := φ ⁻¹' (Metric.ball y ε)
    use U
    constructor
    · exact φ.continuous_invFun.isOpen_preimage _ (Metric.isOpen_ball)
    · constructor
      · simp [U, y]
      · refine ⟨hU_open.inter (φ.continuous_invFun.isOpen_preimage _ Metric.isOpen_ball), ?_⟩
        -- We need to show U is homeomorphic to a ball.
        -- φ restricted to U maps to Metric.ball y ε.
        use y, ε
        let φ_restrict := φ.preimageHomeomorph (Metric.ball y ε)
        use φ_restrict
        trivial

  -- "Because every open cover of a second-countable space has a countable subcover..."
  -- We construct a cover of Coordinate Balls.
  -- Note: Lee proves it by covering M with countably many charts first, then refining.
  
  -- Let ℬ_Rn be a countable basis of precompact balls for ℝⁿ.
  -- Since ℝⁿ is second-countable and locally compact, it has a countable basis of precompact open balls.
  -- For now, we use the fact that it's second-countable to get a countable basis.
  obtain ⟨B_Rn, hB_Rn_count, hB_Rn_basis⟩ := exists_countable_basis (Fin n → ℝ)
  
  -- "M is covered by countably many charts {(Ui, φi)}."
  -- Since M is second-countable, we can extract a countable subcover from the cover of coordinate balls h_cover.
  obtain ⟨S, hS_count, hS_cover⟩ := TopologicalSpace.SecondCountableTopology.is_open_union_countable (Set.range (fun p => (Classical.choose (h_cover p)))) (by
    intro U hU
    obtain ⟨p, rfl⟩ := hU
    exact (Classical.choose_spec (h_cover p)).1)
  
  -- Each coordinate domain Ui in S has a countable basis of coordinate balls.
  -- For each U ∈ S, let φ_U be the homeomorphism to an open subset of ℝⁿ.
  -- Let B_U be the collection of preimages of balls in B_Rn that are contained in φ_U(U).
  let B := ⋃ (U ∈ S), { V | ∃ (B_ball ∈ B_Rn), V = (Classical.choose (Classical.choose_spec (Classical.choose_spec (Classical.choose_spec (h_cover (Classical.choose (Set.mem_range.mp (by sorry)))))))) ⁻¹' B_ball ∧ closure B_ball ⊆ (Classical.choose_spec (Classical.choose_spec (h_cover (Classical.choose (Set.mem_range.mp (by sorry)))))) }

  -- "The union of all these countable bases is a countable basis for the topology of M."
  use B
  constructor
  · -- Countable union of countable sets is countable.
    sorry
  · constructor
    · -- B is a topological basis.
      sorry
    · constructor
      · -- Each V in B is a coordinate ball.
        intro V hV
        sorry
      · -- Each V in B is precompact.
        intro V hV
        sorry
