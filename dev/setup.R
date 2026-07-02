# Setup air
# usethis::use_air()

# Add air GitHub actions

usethis::use_github_action(
  url = "https://github.com/posit-dev/setup-air/blob/main/examples/format-check.yaml"
)
usethis::use_github_action(
  url = "https://github.com/etiennebacher/setup-jarl/blob/main/examples/jarl-check.yml"
)


# Snapshot renv lockfile
renv::status()
renv::update(lock = TRUE)
renv::snapshot(type = "explicit", dev = TRUE)

# Update wordlist
spelling::update_wordlist()

# Write manifest
rsconnect::writeManifest()

# Increment version number
usethis::use_version("dev")
