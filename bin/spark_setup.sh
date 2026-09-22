#!/usr/bin/env bash
# Usage: ./setup_ssh_key.sh <git-repo-url> [key-name]
# Example: ./setup_ssh_key.sh git@github.com:org/repo.git deploy_key

set -euo pipefail

REPO_URL="${1:?Usage: $0 <git-repo-url> [key-name]}"
KEY_NAME="${2:-id_ed25519_deploy}"
KEY_PATH="$HOME/.ssh/${KEY_NAME}"

# 1. Generate a new SSH key (separate from the primary)
if [ ! -f "$KEY_PATH" ]; then
	echo "Generating new SSH key: ${KEY_PATH}"
	ssh-keygen -t ed25519 -f "${KEY_PATH}" -N "" -C "${KEY_NAME}"
fi

echo ""
echo "========================================="
echo " Public key for ${KEY_NAME}:"
echo "========================================="
cat "${KEY_PATH}.pub"
echo "========================================="
echo ""

# 2. Prompt user to add the key before continuing
read -rp "Add the above public key to the repo/account, then press [Enter] to continue..."

# 3. Ensure ssh-agent is running and add the new key
eval "$(ssh-agent -s)"
ssh-add "${KEY_PATH}"

# 4. Configure SSH to use this key for the target host
HOST=$(echo "${REPO_URL}" | sed -E 's|.*@([^:/]+)[:/].*|\1|')
SSH_CONFIG="$HOME/.ssh/config"

if ! grep -qF "Host ${HOST}-${KEY_NAME}" "${SSH_CONFIG}" 2>/dev/null; then
  cat >> "${SSH_CONFIG}" <<EOF

# Added by setup_ssh_key.sh
Host ${HOST}-${KEY_NAME}
    HostName ${HOST}
    User git
    IdentityFile ${KEY_PATH}
    IdentitiesOnly yes
EOF
  chmod 600 "${SSH_CONFIG}"
  echo "SSH config entry added for ${HOST}-${KEY_NAME}"
fi

# 5. Rewrite the repo URL to use the custom SSH host alias
REPO_PATH=$(echo "${REPO_URL}" | sed -E "s|.*@${HOST}[:/]||")
CLONE_URL="git@${HOST}-${KEY_NAME}:${REPO_PATH}"

echo ""
echo "Cloning ${CLONE_URL} ..."
git clone "${CLONE_URL}"

echo ""
echo "Done! Repo cloned successfully using key: ${KEY_PATH}"
