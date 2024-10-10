packer {
  required_plugins {
    googlecompute = {
      version = ">= 1.1.6"
      source  = "github.com/hashicorp/googlecompute"
    }
  }
}

# Define required variables
variable "project_id" {
  type    = string
  default = "arweave-437622"
}

variable "region" {
  type    = string
  default = "us-east1"
}

variable "zone" {
  type    = string
  default = "us-east1-c"
}

variable "image_family" {
  type    = string
  default = "hello-erlang-family"
}

# Source block to define GCP builder
source "googlecompute" "debian" {
  project_id      = var.project_id
  source_image_family = "debian-11"
  zone            = var.zone
  machine_type    = "n1-standard-1"
  ssh_username    = "packer"
}

# Define the build stage
build {
  sources = ["source.googlecompute.debian"]

  # Install Nginx
  provisioner "shell" {
    inline = [
      "sudo DEBIAN_FRONTEND=noninteractive apt-get update",
      "sudo DEBIAN_FRONTEND=noninteractive apt-get install -y nginx"
    ]
  }

  # Configure Nginx as reverse proxy
  provisioner "shell" {
    inline = [
      "sudo bash -c 'cat <<EOF > /etc/nginx/sites-available/www_erl\nserver {\n    listen 80;\n    server_name _;\n    location / {\n        proxy_pass http://localhost:8080/;\n        proxy_set_header Host $host;\n        proxy_set_header X-Real-IP $remote_addr;\n    }\n}\nEOF'"
    ]
  }

  provisioner "shell" {
    inline = [
      "sudo ln -s /etc/nginx/sites-available/www_erl /etc/nginx/sites-enabled/",
      "sudo rm -f /etc/nginx/sites-enabled/default",
      "sudo nginx -t",
      "sudo systemctl restart nginx"
    ]
  }


  # Upload the pre-built release (with ERTS included) to the instance
  provisioner "file" {
    source      = "./_build/default/rel/www_erl"
    destination = "/tmp/www_erl"
  }

  provisioner "shell" {
    inline = [
      # Move the release to /opt with sudo
      "sudo mv /tmp/www_erl /opt/www_erl",
      "sudo chmod -R 755 /opt/www_erl",

      # Create a symlink to make it easier to run the app
      "sudo ln -s /opt/www_erl/bin/www_erl /usr/local/bin/www_erl",

      # (Optional) If you want to create a systemd service to manage the app
      "echo '[Unit]' | sudo tee /etc/systemd/system/www_erl.service",
      "echo 'Description=Hello Erlang App' | sudo tee -a /etc/systemd/system/www_erl.service",
      "echo '[Service]' | sudo tee -a /etc/systemd/system/www_erl.service",
      "echo 'ExecStart=/opt/www_erl/bin/www_erl start' | sudo tee -a /etc/systemd/system/www_erl.service",
      "echo 'Restart=always' | sudo tee -a /etc/systemd/system/www_erl.service",
      "echo '[Install]' | sudo tee -a /etc/systemd/system/www_erl.service",
      
      # Enable and start the service
      "sudo systemctl enable www_erl",
      "sudo systemctl start www_erl"
    ]
  }
}

